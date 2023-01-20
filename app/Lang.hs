module Lang where

import Text.Printf (printf)

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

instance Show Re where
  show re = case re of
    Nil -> "∅"
    Eps -> "ε"
    Dot -> "."
    Sym c -> [c]
    Seq r s -> "(" ++ show r ++ show s ++ ")"
    Alt r s -> "(" ++ show r ++ "|" ++ show s ++ ")"
    And r s -> "(" ++ show r ++ "&" ++ show s ++ ")"
    Not r -> "!" ++ show r
    Str r -> show r ++ "*"

-- ∅|a == a   a|∅ == a
rAlt :: Re -> Re -> Re
rAlt Nil r = r
rAlt r Nil = r
rAlt r s   = Alt r s

-- ∅&a == ∅   a&∅ == ∅
rAnd :: Re -> Re -> Re
rAnd Nil _ = Nil
rAnd _ Nil = Nil
rAnd r s   = And r s

-- ∅a == ∅   a∅ == ∅   εa == a   aε == a
rSeq :: Re -> Re -> Re
rSeq Nil _ = Nil
rSeq _ Nil = Nil
rSeq Eps r = r
rSeq r Eps = r
rSeq r s   = Seq r s

-- (r*)* == r*   ∅* == ε   ε* == ε
rStr :: Re -> Re
rStr Eps = Eps
rStr Nil = Eps
rStr (Str r) = rStr r
rStr r       = Str r

-- !(!r) == r
rNot :: Re -> Re
rNot (Not r) = r
rNot r       = Not r

-- r? == r|ε
rMay :: Re -> Re
rMay r = rAlt r Eps

-- r+ == rr*
rPls :: Re -> Re
rPls r = rSeq r (rStr r)

-- r = s == (r&s)|(!r&!s)
rIff :: Re -> Re -> Re
rIff r s = rAlt (rAnd r s) (rAnd (rNot r) (rNot s))

-- r ^ s == !(r=s)
rXor :: Re -> Re -> Re
rXor r s = rNot (rIff r s)

-- r > s == !r|s
rImp :: Re -> Re -> Re
rImp r s = rAlt (rNot r) s

-- [a-z] == (a|b|...|z)
rRng :: Char -> Char -> Re
rRng a b = foldr (rAlt . Sym) Nil [a..b]