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

import Data.Conduit
import qualified Data.Conduit.List as CL

import Safe

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch
import qualified Network.IRC as IRC

import System.Random
import System.FilePath
import System.Directory
import System.Environment.XDG.BaseDir
import System.Locale

newtype QuoteId_v0 = QuoteId_v0 { unQuoteId_v0 :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)

newtype QuoteId = QuoteId { unQuoteId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable)
$(deriveSafeCopy 1 'extension ''QuoteId)

instance Migrate QuoteId where
    type MigrateFrom QuoteId = QuoteId_v0
    migrate (QuoteId_v0 i) = QuoteId i

newtype Author_v0 = Author_v0 T.Text
    deriving (Eq, Ord, Data, Typeable, SafeCopy)

newtype QuoteAuthor = QuoteAuthor {unQuoteAuthor :: T.Text}
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 1 'extension ''QuoteAuthor)

instance Migrate QuoteAuthor where
    type MigrateFrom QuoteAuthor = Author_v0
    migrate (Author_v0 a) = QuoteAuthor a

newtype QuoteTime = QuoteTime {unQuoteTime :: UTCTime}
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 1 'extension ''QuoteTime)

instance Migrate QuoteTime where
    type MigrateFrom QuoteTime = UTCTime
    migrate t = QuoteTime t

newtype QuoteText = QuoteText {unQuoteText :: T.Text}
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 1 'extension ''QuoteText)

instance Migrate QuoteText where
    type MigrateFrom QuoteText = T.Text
    migrate t = QuoteText t

data Quote_v0 = Quote_v0 {
    quoteId_v0 :: QuoteId_v0,
    quoteTime_v0 :: UTCTime,
    quoteAuthor_v0 :: T.Text,
    quoteText_v0 :: T.Text
} deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Quote_v0)

data Quote = Quote {
    quoteId :: QuoteId,
    quoteTime :: QuoteTime,
    quoteAuthor :: QuoteAuthor,
    quoteText :: QuoteText
} deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 1 'extension ''Quote)

instance Migrate Quote where
    type MigrateFrom Quote = Quote_v0
    migrate (Quote_v0 i t a q) = Quote (QuoteId . unQuoteId_v0 $ i) (QuoteTime t) (QuoteAuthor a) (QuoteText q)

instance Indexable Quote_v0 where
    empty = ixSet
        [ ixFun $ \q -> [quoteId_v0 q],
          ixFun $ \q -> [Author_v0 (quoteAuthor_v0 q)]
        ]

data Quotes_v0 = Quotes_v0 {
    nextQuoteId_v0 :: QuoteId_v0,
    quotes_v0 :: IxSet Quote_v0
} deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Quotes_v0)

instance Indexable Quote where
    empty = ixSet
        [ ixFun $ \q -> [quoteId q],
          ixFun $ \q -> [quoteAuthor q]
        ]

data Quotes = Quotes {
    nextQuoteId :: QuoteId,
    quotes :: IxSet Quote
} deriving (Data, Typeable)

$(deriveSafeCopy 1 'extension ''Quotes)

instance Migrate Quotes where
    type MigrateFrom Quotes = Quotes_v0
    migrate (Quotes_v0 i qs) = Quotes i' qs'
        where
            i'  = QuoteId . unQuoteId_v0 $ i
            qs' = IX.fromList . map migrateQuotes_v0 . IX.toList $ qs
            migrateQuotes_v0 q0 = migrate q0

initialQuotesState :: Quotes
initialQuotesState = Quotes { nextQuoteId = QuoteId 1, quotes = IX.empty }

addQuote :: UTCTime -> T.Text -> T.Text -> Update Quotes Integer
addQuote time author text = do
    qs <- get
    let quote = Quote {
            quoteId     = nextQuoteId qs,
            quoteTime   = QuoteTime time,
            quoteAuthor = QuoteAuthor author,
            quoteText   = QuoteText text }

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
    let q = quotes qs @= QuoteAuthor author

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
    if Prelude.null qs then
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
    case q of
        Just q' -> do
                    let qId = show (unQuoteId (quoteId q'))
                        author' = unQuoteAuthor $ quoteAuthor q'
                        time = formatTime defaultTimeLocale "%F %R UTC" (unQuoteTime . quoteTime $ q')
                        text = unQuoteText $ quoteText q'
                        s = "-- Quote " ++ qId ++ ", " ++ T.unpack author' ++ " at " ++ time
                    return (Just (T.unpack text, s))
        _       -> return Nothing


quotesCommandHandler :: MessageHandler
quotesCommandHandler inChan outChan = do
    dir <- quotesDbPath
    bracket (liftIO $ openLocalStateFrom dir initialQuotesState)
        (liftIO . createCheckpointAndClose)
        run
    where
        quotesDbPath = do
            baseDir <- liftIO $ getUserDataDir "duckduckbot"
            server <- asks messageHandlerEnvServer
            nick <- asks messageHandlerEnvNick
            channel <- asks messageHandlerEnvChannel
            let dir = baseDir </> server </> nick </> channel </> "quotes"
            liftIO $ createDirectoryIfMissing True dir
            return dir
        run acid = sourceChan inChan
                       =$= takeIRCMessage
                       =$= CL.concatMapM (handleQuoteCommand acid)
                       $$ CL.map OutIRCMessage
                       =$= sinkChan outChan

handleQuoteCommand :: MonadIO m => AcidState Quotes -> IRC.Message -> m [IRC.Message]

handleQuoteCommand acid m | isQuoteAddCommand m, (Just target) <- maybeGetPrivMsgReplyTarget m = handleQuoteAdd target
    where
        isQuoteAddCommand = isPrivMsgCommand "quote-add"

        handleQuoteAdd target = do
            let cmd = (T.stripStart . T.decodeUtf8With T.lenientDecode . B.drop 11 . parseCommand) m
                (author, quote') = T.break isSpace cmd
                quote = T.strip quote'
            if author /= T.empty && quote /= T.empty then do
                time <- liftIO getCurrentTime
                qId <- update' acid (AddQuote time author quote)
                return [quoteMessage target (UB.fromString ("Added quote " ++ show qId))]
            else
                return []

handleQuoteCommand acid m | isQuoteRmCommand m, (Just target) <- maybeGetPrivMsgReplyTarget m  = handleQuoteRm target
    where
        isQuoteRmCommand = isPrivMsgCommand "quote-rm"

        handleQuoteRm target = do
            let cmd = (T.strip . T.decodeUtf8With T.lenientDecode . B.drop 10 . parseCommand) m
                qId = (readMay . T.unpack) cmd
            case qId of
                (Just qId') -> do
                                    _ <- update' acid (RmQuote (QuoteId qId'))
                                    return [quoteMessage target (UB.fromString ("Removed quote " ++ show qId'))]
                _           -> return []


handleQuoteCommand acid m | isQuoteCommand m, (Just target) <- maybeGetPrivMsgReplyTarget m = handleQuote target
    where
        isQuoteCommand = isPrivMsgCommand "quote"

        handleQuote target = do
            let author = (T.strip . T.decodeUtf8With T.lenientDecode . B.drop 7 . parseCommand) m
            q <- getQuoteString acid author
            case q of
                Just (s',t')                -> return [quoteMessage target (UB.fromString s'), quoteMessage target (UB.fromString t')]
                Nothing | author /= T.empty -> return [quoteMessage target (UB.fromString ("No quote by " ++ T.unpack author))]
                _                           -> return []

handleQuoteCommand _ _ = return []

parseCommand :: IRC.Message -> B.ByteString
parseCommand (IRC.Message _ _ (_:cmd:[])) = cmd
parseCommand _                            = B.empty

quoteMessage :: B.ByteString -> B.ByteString -> IRC.Message
quoteMessage target s = IRC.Message { IRC.msg_prefix = Nothing,
                                      IRC.msg_command = "PRIVMSG",
                                      IRC.msg_params = [target, s]
                                    }

quotesCommandHandlerMetadata :: MessageHandlerMetadata
quotesCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "quotes",
    messageHandlerMetadataCommands = ["!quote", "!quote-add", "!quote-rm"],
    messageHandlerMetadataHandler = quotesCommandHandler
}

