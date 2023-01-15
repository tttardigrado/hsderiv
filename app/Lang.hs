module Lang where

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
rMay :: Re -> Re
rMay r = Alt r Eps

-- r+ == rr*
rPls :: Re -> Re
rPls r = Seq r (Str r)

rStr :: Re -> Re
rStr re = case re of
  Eps   -> Eps
  Nil   -> Eps
  Str r -> rStr r
  _     -> Str re

rNot :: Re -> Re
rNot re = case re of
  Not r -> r
  _     -> Not re

rAlt :: Re -> Re -> Re
rAlt re1 re2 = case (re1, re2) of
  (Nil,     r)    -> r
  (Not Nil, r)    -> Not Nil
  (r, s) | r == s -> r
  _               -> Alt re1 re2

rAnd :: Re -> Re -> Re
rAnd re1 re2 = case (re1, re2) of
  (Nil,     r)    -> Nil
  (Not Nil, r)    -> r
  (r, s) | r == s -> r
  _               -> And re1 re2

rSeq :: Re -> Re -> Re
rSeq re1 re2 = case (re1, re2) of
  (Nil,     r) -> Nil
  (r,     Nil) -> Nil
  (Eps,     r) -> r
  (r,     Eps) -> r
  _            -> Seq re1 re2
