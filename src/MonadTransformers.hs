{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MonadTransformers where

import Control.Monad.Reader
import           Control.Monad.IO.Class


newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (Monad f, Monad g) => Monad (Compose f g) where
  return = pure
  (>>=) :: Compose f g a
        -> (a -> Compose f g b)
        -> Compose f g b
  (>>=) = undefined











newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (>>=) (IdentityT ma) f = IdentityT $ ma >>= runIdentityT . f














newtype MySimpleApp a = MySimpleApp { unMySimpleApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)


newtype MyRealApp a = MyRealApp { unMyRealApp :: ReaderT String IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader String)



myMain :: IO ()
myMain = runReaderT (unMyRealApp mainLogic) "db conn string"


mainLogic :: MyRealApp ()
mainLogic = do
  stuff <- doThingOne
  things <- doThingTwo stuff
  pure ()

doThingOne :: MonadReader String m => m Int
doThingOne = undefined

doThingTwo :: Monad m => Int -> m String
doThingTwo = undefined
































































instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)


instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = undefined

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = undefined
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) = undefined
