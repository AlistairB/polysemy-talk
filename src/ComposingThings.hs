{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
module ComposingThings where

import Control.Monad.IO.Class
import Data.Functor.Identity




data User = User String


class Monad m => DBTalker m where
  fetchUserFromDB' :: String -> m User
  updateUserInDB' :: String -> User -> m ()

setUserName :: String -> User -> User
setUserName = undefined

updateUserNameInDb' :: DBTalker m => String -> String -> m ()
updateUserNameInDb' userId userName = do
    user <- fetchUserFromDB' userId
    let updatedUser = setUserName userName user
    result <- updateUserInDB' userId updatedUser
    pure result






class Monad m => MonadReader r m | m -> r where
  ask :: m r


maybeUpdateUserNameInDbAndLog :: (MonadReader String m, DBTalker m) => String -> String -> m ()
maybeUpdateUserNameInDbAndLog = do
    connectionString <- ask
    db <- connectDb connectionString
    user <- fetchUserFromDB' db userId
    let updatedUser = setUserName userName user
    result <- updateUserInDB' db userId updatedUser
    pure result









data ReaderIO env a = ReaderIO { runReaderIO :: env -> IO a }

newtype Reader env a = Reader { runReader :: env -> a }

newtype MyRealApp' a = MyRealApp' { unMyRealApp :: Reader String (IO a) }
  deriving (Functor, Applicative, Monad, MonadIO)

-- instance DBTalker MyRealApp' where
--   fetchUserFromDB' userId = liftIO $ reallyFetchFromDBInIO userId
--   updateUserInDB' userId user = liftIO $ reallyUpdateDBInIO userId user


reallyFetchFromDBInIO :: String -> IO User
reallyFetchFromDBInIO = undefined

reallyUpdateDBInIO :: String -> User -> IO ()
reallyUpdateDBInIO = undefined

instance MonadReader String MyRealApp' where
  ask = MyRealApp' $ Reader $ \dbConn -> pure dbConn
