module DuckDuckBot.Compat (
#if !MIN_VERSION_base(4,6,0)
    forkFinally,
    lookupEnv
#endif
) where

#if !MIN_VERSION_base(4,6,0)
import Control.Exception
import Control.Concurrent
import System.IO.Error hiding (try)
import System.Environment

lookupEnv :: String -> IO (Maybe String)
lookupEnv k =
    let checkException e | isDoesNotExistError e = return Nothing
                         | otherwise             = throwIO e
    in handle checkException (getEnv k >>= return . Just)

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
#endif
