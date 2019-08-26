{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ThePowerOfTypeClasses where

import Control.Monad.IO.Class
import Data.Functor.Identity




data User = User String


updateUserNameInDb :: String -> String -> IO ()
updateUserNameInDb userId userName = do
    user <- fetchUserFromDB userId
    let updatedUser = setUserName userName user
    result <- updateUserInDB userId updatedUser
    pure result


fetchUserFromDB :: String -> IO User
fetchUserFromDB = undefined

updateUserInDB :: String -> User -> IO ()
updateUserInDB = undefined

setUserName :: String -> User -> User
setUserName = undefined








class Monad m => DBTalker m where
  fetchUserFromDB' :: String -> m User
  updateUserInDB' :: String -> User -> m ()




updateUserNameInDb' :: DBTalker m => String -> String -> m ()
updateUserNameInDb' userId userName = do
    user <- fetchUserFromDB' userId
    let updatedUser = setUserName userName user
    result <- updateUserInDB' userId updatedUser
    pure result


newtype MyRealApp a = MyRealApp { unMyRealApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance DBTalker MyRealApp where
  fetchUserFromDB' userId = liftIO $ reallyFetchFromDBInIO userId
  updateUserInDB' userId user = liftIO $ reallyUpdateDBInIO userId user


-- liftIO :: IO a -> m a

reallyFetchFromDBInIO :: String -> IO User
reallyFetchFromDBInIO = undefined

reallyUpdateDBInIO :: String -> User -> IO ()
reallyUpdateDBInIO = undefined









newtype Identity' a = Identity' a


newtype MyTestApp a = MyTestApp { unMyTestApp :: Identity a }
  deriving (Functor, Applicative, Monad)

instance DBTalker MyTestApp where
  fetchUserFromDB' _ = MyTestApp $ Identity $ User "Bob"
  updateUserInDB' _ _ = MyTestApp $ Identity ()
