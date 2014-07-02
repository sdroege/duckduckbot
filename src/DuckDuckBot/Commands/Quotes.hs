{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module DuckDuckBot.Commands.Quotes (
  quotesCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import Data.IxSet as IX

import Data.Acid
import Data.Acid.Local
import Data.Acid.Advanced
import Data.SafeCopy
import Data.Data
import Data.Char
import Data.Time
import Data.Maybe

import Safe

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch
import Control.Concurrent.Chan
import qualified Network.IRC as IRC

import System.Random
import System.FilePath
import System.Directory
import System.Environment.XDG.BaseDir
import System.Locale

newtype QuoteId = QuoteId { unQuoteId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)

data Quote = Quote {
    quoteId :: QuoteId,
    quoteTime :: UTCTime,
    quoteAuthor :: T.Text,
    quoteText :: T.Text
} deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Quote)

newtype Author = Author T.Text
    deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Indexable Quote where
    empty = ixSet
        [ ixFun $ \q -> [quoteId q],
          ixFun $ \q -> [Author (quoteAuthor q)]
        ]

data Quotes = Quotes {
    nextQuoteId :: QuoteId,
    quotes :: IxSet Quote
} deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Quotes)

initialQuotesState :: Quotes
initialQuotesState = Quotes { nextQuoteId = QuoteId 1, quotes = IX.empty }

addQuote :: UTCTime -> T.Text -> T.Text -> Update Quotes Integer
addQuote time author text = do
    qs <- get
    let quote = Quote {
            quoteId     = nextQuoteId qs,
            quoteTime   = time,
            quoteAuthor = author,
            quoteText   = text }

    put $ qs {  nextQuoteId = succ (nextQuoteId qs),
                quotes      = insert quote (quotes qs)
             }
    return (unQuoteId (nextQuoteId qs))

rmQuote :: QuoteId -> Update Quotes ()
rmQuote k = do
    qs <- get

    put $ qs { quotes = deleteIx k (quotes qs) }

getQuote :: Integer -> Query Quotes (Maybe Quote)
getQuote k = do
    qs <- ask
    let q = quotes qs @= QuoteId k
    return (getOne q)

getNumQuotes :: Query Quotes Integer
getNumQuotes = do
    qs <- ask
    return (pred (unQuoteId (nextQuoteId qs)))

getQuotesByAuthor :: T.Text -> Query Quotes [Quote]
getQuotesByAuthor author = do
    qs <- ask
    let q = quotes qs @= Author author

    return (toList q)

$(makeAcidic ''Quotes
  [ 'addQuote
  , 'rmQuote
  , 'getQuote
  , 'getNumQuotes
  , 'getQuotesByAuthor
  ])

getRandomQuoteByAuthor :: MonadIO m => T.Text -> AcidState Quotes -> m (Maybe Quote)
getRandomQuoteByAuthor author acid = do
    qs <- query' acid (GetQuotesByAuthor author)
    if qs == [] then
        return Nothing
    else
        do
            idx <- liftIO $ randomRIO (0, pred $ length qs)
            return $ Just (qs !! idx)

getRandomQuote :: MonadIO m => AcidState Quotes -> m (Maybe Quote)
getRandomQuote acid = do
    n <- query' acid GetNumQuotes
    if n == 0 then
        return Nothing
    else
        do
            idx <- liftIO $ randomRIO (1, n)
            q <- query' acid (GetQuote idx)
            case q of
                Nothing -> getRandomQuote acid
                _       -> return q

getQuoteString :: MonadIO m => AcidState Quotes -> T.Text -> m (Maybe (String, String))
getQuoteString acid author = do
    q <- if author == "" then getRandomQuote acid else getRandomQuoteByAuthor author acid
    if isJust q then
        do
            let q' = fromJust q
                qId = show (unQuoteId (quoteId q'))
                author' = quoteAuthor q'
                time = formatTime defaultTimeLocale "%F %R UTC" (quoteTime q')
                text = quoteText q'
                s = "-- Quote " ++ qId ++ ", " ++ T.unpack author' ++ " at " ++ time
            return (Just (T.unpack text, s))
    else
        return Nothing

runQuotesHandler :: AcidState Quotes -> MessageHandler
runQuotesHandler acid cIn cOut =
    untilFalse $ do
        msg <- liftIO $ readChan cIn
        case msg of
            InIRCMessage m | isQuoteAddCommand m -> handleQuoteAdd m >> return True
            InIRCMessage m | isQuoteRmCommand  m -> handleQuoteRm m >> return True
            InIRCMessage m | isQuoteCommand    m -> handleQuote m >> return True
            Quit                                 -> return False
            _                                    -> return True
    where
        isQuoteAddCommand = isPrivMsgCommand "quote-add"
        isQuoteRmCommand = isPrivMsgCommand "quote-rm"
        isQuoteCommand = isPrivMsgCommand "quote"

        parseCommand m = cmd
                         where params = IRC.msg_params m
                               cmd    = head (tail params)

        handleQuoteAdd m = when (target /= B.empty) $ do
                                let cmd = (T.stripStart . T.decodeUtf8With T.lenientDecode . B.drop 11 . parseCommand) m
                                    (author, quote') = T.break isSpace cmd
                                    quote = T.strip quote'
                                when (author /= T.empty && quote /= T.empty) $ do
                                    time <- liftIO getCurrentTime
                                    qId <- update' acid (AddQuote time author quote)
                                    sendQuoteMessage target ("Added quote " ++ show qId)
                           where
                                target = getPrivMsgReplyTarget m

        handleQuoteRm  m = when (target /= B.empty) $ do
                                let cmd = (T.strip . T.decodeUtf8With T.lenientDecode . B.drop 10 . parseCommand) m
                                    qId = (readMay . T.unpack) cmd
                                when (isJust qId) $ do
                                    let qId' = fromJust qId
                                    _ <- update' acid (RmQuote (QuoteId qId'))
                                    sendQuoteMessage target ("Removed quote " ++ show qId')
                           where
                                target = getPrivMsgReplyTarget m

        handleQuote    m = when (target /= B.empty) $ do
                                let author = (T.strip . T.decodeUtf8With T.lenientDecode . B.drop 7 . parseCommand) m
                                q <- getQuoteString acid author
                                case q of
                                    Just (s',t')                -> sendQuoteMessage target s' >> sendQuoteMessage target t'
                                    Nothing | author /= T.empty -> sendQuoteMessage target ("No quote by " ++ T.unpack author)
                                    _                           -> return ()
                           where
                                target = getPrivMsgReplyTarget m

        sendQuoteMessage target s = liftIO $ writeChan cOut (quoteMessage target (UB.fromString s))

        quoteMessage target s = OutIRCMessage IRC.Message { IRC.msg_prefix = Nothing,
                                                            IRC.msg_command = "PRIVMSG",
                                                            IRC.msg_params = [target, s]
                                                          }

quotesCommandHandler :: MessageHandler
quotesCommandHandler cIn cOut = do
    dir <- quotesDbPath
    bracket (liftIO $ openLocalStateFrom dir initialQuotesState)
        (liftIO . createCheckpointAndClose)
        (\acid -> runQuotesHandler acid cIn cOut)
    where
        quotesDbPath = do
            baseDir <- liftIO $ getUserDataDir "duckduckbot"
            server <- asks messageHandlerEnvServer
            nick <- asks messageHandlerEnvNick
            channel <- asks messageHandlerEnvChannel
            let dir = baseDir </> server </> nick </> channel
            liftIO $ createDirectoryIfMissing True dir
            return dir

quotesCommandHandlerMetadata :: MessageHandlerMetadata
quotesCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "quotes",
    messageHandlerMetadataCommands = ["!quote", "!quote-add", "!quote-rm"],
    messageHandlerMetadataHandler = quotesCommandHandler
}

