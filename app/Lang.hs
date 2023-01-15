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
may :: Re -> Re
may r = Alt r Eps

-- r+ == rr*
pls :: Re -> Re
pls r = Seq r (Str r)