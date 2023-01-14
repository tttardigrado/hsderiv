module Lang where

data Re
  = Ept       -- ∅
  | Eps       -- ε
  | Dot       -- .
  | Sym Char  -- a
  | Seq Re Re -- r ⋅ s
  | Alt Re Re -- r | s
  | And Re Re -- r & s
  | Not Re    -- ¬r
  | Str Re    -- r*
  deriving (Show, Eq)