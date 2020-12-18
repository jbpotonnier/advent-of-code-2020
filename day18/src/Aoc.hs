module Aoc
  ( module Aoc,
  )
where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

data Exp
  = Add Exp Exp
  | Mult Exp Exp
  | Number Int
  deriving (Show, Eq)

type Parser = Parsec Void Text

eval :: Exp -> Int
eval = \case
  (Add a b) -> eval a + eval b
  (Mult a b) -> eval a * eval b
  (Number n) -> n

expP :: Parser Exp
expP = makeExprParser termP operatorTable
  where
    operatorTable = [[binary "*" Mult, binary "+" Add]]
    termP = choice [parens expP, numberP]

exp2P :: Parser Exp
exp2P = makeExprParser termP operatorTable
  where
    operatorTable = [[binary "+" Add], [binary "*" Mult]]
    termP = choice [parens exp2P, numberP]

binary :: Text -> (a -> a -> a) -> Operator (ParsecT Void Text Identity) a
binary name f = InfixL (f <$ symbol name)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

numberP :: Parser Exp
numberP = Number <$> intP

intP :: Parser Int
intP = lexeme Lexer.decimal

spaceConsumer :: Parser ()
spaceConsumer =
  Lexer.space
    space1
    (Lexer.skipLineComment "//")
    (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

readInput :: (MonadIO f) => Parser b -> FilePath -> f [b]
readInput p path = mapMaybe (parseMaybe p) . lines <$> readFileText path
