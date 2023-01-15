module Derivative where

import Lang (Re (..))

-- Does it accept ε
δ :: Re -> Bool
δ re = case re of
  Nil     -> False      -- doesn't accept anything
  Eps     -> True       -- only accepts ε
  Dot     -> False      -- only accepts symbols
  Sym c   -> False      -- only accepts c
  Seq r s -> δ r && δ s -- if both accept ε
  Alt r s -> δ r || δ s -- if one accepts ε
  And r s -> δ r && δ s -- if both accept ε
  Not r   -> not (δ r)  -- if r doesn't accept ε
  Str r   -> True       -- accepts by definition

-- Brzozowski derivative relative to a character
deriv :: Char -> Re -> Re
deriv c re = case re of
  Nil           -> Nil
  Eps           -> Nil
  Dot           -> Eps
  Sym a         -> if a == c then Eps else Nil
  Seq r s | δ r -> Alt (dc s) $ Seq (dc r) s
  Seq r s       -> Seq (dc r) s
  Alt r s       -> Alt (dc r) (dc s)
  And r s       -> And (dc r) (dc s)
  Not r         -> Not (dc r)
  Str r         -> Seq (dc r) (Str r)
  where
    dc = deriv c

-- Check if a string matches a regular expression
matches :: String -> Re -> Bool
matches str r = case str of
  []  -> δ r                   -- matches empty string?
  a:w -> matches w $ deriv a r -- match to the derivative