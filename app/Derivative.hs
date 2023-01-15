module Derivative where

import Lang ( Re(..) )

-- Does it accept ε
δ :: Re -> Bool
δ re = case re of
  Nil -> False          -- doesn't accept anything
  Eps -> True           -- only accepts ε
  Dot -> False          -- only accepts symbols
  Sym c -> False        -- only accepts c
  Seq r s -> δ r && δ s -- if both accept ε
  Alt r s -> δ r || δ s -- if one accepts ε
  And r s -> δ r && δ s -- if both accept ε
  Not r -> not $ δ r    -- if r doesn't accept ε
  Str r -> True         -- accepts by definition