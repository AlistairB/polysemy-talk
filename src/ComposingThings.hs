{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
module ComposingThings where

import Control.Monad.IO.Class
import Data.Functor.Identity
-- import Control.Monad.Reader


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






class Monad m => MonadReader r m where
  ask :: m r


-- maybeUpdateUserNameInDbAndLog :: (MonadReader String m, DBTalker m) => String -> String -> m ()
-- maybeUpdateUserNameInDbAndLog = do
--     connectionString <- ask
--     db <- connectDb connectionString
--     user <- fetchUserFromDB' db userId
--     let updatedUser = setUserName userName user
--     result <- updateUserInDB' db userId updatedUser
--     pure result









data ReaderIO env a = ReaderIO { runReaderIO :: env -> IO a }

newtype Reader env a = Reader { runReader :: env -> a }
  deriving (Functor)

newtype MyRealApp' a = MyRealApp' { unMyRealApp' :: Reader String (IO a) }
  -- deriving (Functor, Applicative, Monad, MonadIO)

-- instance DBTalker MyRealApp' where
--   fetchUserFromDB' userId = liftIO $ reallyFetchFromDBInIO userId
--   updateUserInDB' userId user = liftIO $ reallyUpdateDBInIO userId user

instance Functor MyRealApp' where
  fmap f (MyRealApp' (Reader envToIOA)) = undefined


reallyFetchFromDBInIO :: String -> IO User
reallyFetchFromDBInIO = undefined

reallyUpdateDBInIO :: String -> User -> IO ()
reallyUpdateDBInIO = undefined













newtype Compose f g a = Compose { getCompose :: f (g a) }



blah :: Compose Maybe Identity Int
blah = Compose $ Just $ Identity 1



instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = undefined
















instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = undefined
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) = undefined










instance (Monad f, Monad g) => Monad (Compose f g) where
  return = pure
  (>>=) :: Compose f g a
        -> (a -> Compose f g b)
        -> Compose f g b
  (>>=) = undefined

  -- join :: (Monad f, Monad g) => f (g (f (g a))) -> f (g a)
  -- join = undefined
