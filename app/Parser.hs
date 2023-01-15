module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Lang

reserved :: [Char]
reserved = "().*+?!&|\\"

parser :: Parser Re
parser = buildExpressionParser table lit

table :: [[Operator Char st Re]]
table = 
    [ [ Postfix (Str <$ char '*')
      , Postfix (may <$ char '?')
      , Postfix (pls <$ char '+')
      , Prefix  (Not <$ char '!')
      ]
    , [ Infix   (return Seq     ) AssocLeft
      ]  
    , [ Infix   (Alt <$ char '|') AssocLeft
      , Infix   (And <$ char '&') AssocLeft
      ]
    ]

lit :: Parser Re
lit = sym <|> dot <|> paren

paren :: Parser Re
paren = between (char '(') (char ')') parser

sym :: Parser Re
sym = Sym <$> (symbols <|> escaped)
  where
    escaped = char '\\' *> oneOf reserved
    symbols = noneOf reserved

dot :: Parser Re
dot = Dot <$ char '.'