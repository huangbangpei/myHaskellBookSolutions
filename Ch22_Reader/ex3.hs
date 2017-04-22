-- P813
{-# LANGUAGE InstanceSigs #-}

newtype Reader r a =
  Reader { getReader :: r -> a }

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb =
    join $ Reader $ \r -> aRb (ra r)
  -- or
  -- (Reader ra) >>= aRb =
  --   Reader $ \r -> getReader (aRb (ra r)) r

getDogRM :: Reader Person Dog
getDogRM = do
  name <- Reader dogName
  addy <- Reader address
  return $ Dog name addy
