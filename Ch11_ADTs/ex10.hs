-- P423

-- 1.
data Quad =
	One
  | Two
  | Three
  | Four
  deriving (Eq, Show)
-- how many different forms can this take?
-- 8
eQuad :: Either Quad Quad
eQuad = Right One

eQuad1 :: Either Quad Quad
eQuad1 = Right Two

-- and so on...

-- 2.
prodQuad :: (Quad, Quad)

-- It takes 4*4 = 16 forms


-- 3.
funcQuad :: Quad -> Quad

-- It takes 4^4 = 256 forms

-- 4.
prodTBool :: (Bool, Bool, Bool)

-- It takes 2 * 2 * 2 = 8 forms

-- 5.
gTwo :: Bool -> Bool -> Bool

-- It takes 2 ^ (2 * 2) = 16 forms

-- 6.
fTwo :: Bool -> Quad -> Quad

-- It takes 4 ^ (4 * 2) = 65536 forms
