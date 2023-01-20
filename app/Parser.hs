module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Lang

parseRegex :: String -> Either ParseError Re
parseRegex = parse (parser <* eof) ""

reserved :: String
reserved = "()[-].*+?!&|^=>\\"

parser :: Parser Re
parser = buildExpressionParser table pLit

table :: [[Operator Char st Re]]
table = 
    [ [ Postfix (rStr <$ char '*')
      , Postfix (rMay <$ char '?')
      , Postfix (rPls <$ char '+')
      ]
    , [ Prefix  (rNot <$ char '!')
      ]
    , [ Infix   (return rSeq     ) AssocLeft
      ]
    , [ Infix   (rAnd <$ char '&') AssocLeft
      ]
    , [ Infix   (rAlt <$ char '|') AssocLeft
      ]
    , [ Infix   (rXor <$ char '^') AssocLeft
      , Infix   (rIff <$ char '=') AssocLeft
      , Infix   (rImp <$ char '>') AssocLeft
      ]
    ]

pLit :: Parser Re
pLit = pRng <|> pSym <|> pDot <|> pParen

pParen :: Parser Re
pParen = between (char '(') (char ')') parser

pSym :: Parser Re
pSym = Sym <$> (symbols <|> escaped)
  where
    escaped = char '\\' *> oneOf reserved
    symbols = noneOf reserved

-- .
pDot :: Parser Re
pDot = Dot <$ char '.'

-- [a-z] => (a|b|...|z)
pRng :: Parser Re
pRng = do
  char '['
  Sym a <- pSym
  char '-'
  Sym b <- pSym
  char ']'
  return $ rRng a b