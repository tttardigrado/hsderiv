module Lang where

import Data.Char (ord, chr)

data Re
  = Nil       -- ∅
  | Eps       -- ε
  | Dot       -- .
  | Sym Char  -- a
  | Seq Re Re -- r ⋅ s
  | Alt Re Re -- r | s
  | And Re Re -- r & s
  | Not Re    -- ¬r
  | Str Re    -- r*
  deriving (Show, Eq)

-- r? == r|ε
may :: Re -> Re
may r = Alt r Eps

-- r+ == rr*
pls :: Re -> Re
pls r = Seq r (Str r)

-- r = s == (r&s)|(!r&!s)
iff :: Re -> Re -> Re
iff r s = Alt (And r s) (And (Not r) (Not s))

-- r ^ s == !(r=s)
xor :: Re -> Re -> Re
xor r s = Not (iff r s)

-- r > s == !r|s
imp :: Re -> Re -> Re
imp r s = Alt (Not r) s


rng :: Char -> Char -> Re
rng a b = foldr (Alt . Sym) Nil [a..b]