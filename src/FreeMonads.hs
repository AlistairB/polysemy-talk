module FreeMonads where

class Applicative m => Monad' m where
  return :: a -> m a
  join :: m (m a) -> m a

data Free f a =
    Pure a
  | Free (f (Free f a))
