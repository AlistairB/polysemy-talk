module MonadsAndYou where

class Applicative m => Monad' m where
  (>>=) :: m a -> (a -> m b) -> m b
  join :: m (m a) -> m a


{-
  (>>=) = join . fmap

  m a

  m (a -> m b)

  m (m b)
-}



maybeConcatStrings :: Maybe String -> Maybe String -> Maybe String
maybeConcatStrings thing1 thing2 = do
    something <- thing1
    anotherThing <- thing2
    pure $ something ++ anotherThing














concatInM :: Monad m => m String -> m String -> m String
concatInM thing1 thing2 = do
    something <- thing1
    anotherThing <- thing2
    pure $ something ++ anotherThing
