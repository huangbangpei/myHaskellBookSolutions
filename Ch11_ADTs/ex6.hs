-- P398

-- 1.
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

-- BigSmall's cardinality is 4

-- 2.
data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

-- NumberOrBool's cardinality is 256 + 2 = 258
-- if you write like this:

Numba (129)

-- you will get a wearning:
-- "Literal 129 is out of the Int8 range -128..127"