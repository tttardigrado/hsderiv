module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Lang


parser :: Parser Re
parser = buildExpressionParser table lit

table :: [[Operator Char st Re]]
table = 
    [ [ Postfix (rStr <$ char '*')
      , Postfix (rMay <$ char '?')
      , Postfix (rPls <$ char '+')
      , Prefix  (rNot <$ char '!')
      ]
    , [ Infix (return rSeq     ) AssocLeft
      ]
    , [ Infix (rAlt <$ char '|') AssocLeft
      , Infix (rAnd <$ char '&') AssocLeft
      ]
    ]

lit :: Parser Re
lit = dot <|> sym <|> paren

paren :: Parser Re
paren = between (char '(') (char ')') parser

sym :: Parser Re
sym = Sym <$> noneOf "().*!&|"

dot :: Parser Re
dot = Dot <$ char '.'