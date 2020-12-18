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

compute :: Text -> Maybe Int
compute = fmap eval . parseExp

compute2 :: Text -> Maybe Int
compute2 = fmap eval . parseExp2

eval :: Exp -> Int
eval = \case
  (Add a b) -> eval a + eval b
  (Mult a b) -> eval a * eval b
  (Number n) -> n

parseExp :: Text -> Maybe Exp
parseExp = parseMaybe expP

parseExp2 :: Text -> Maybe Exp
parseExp2 = parseMaybe expP2

termP :: Parser Exp
termP = choice [parens expP, numberP]

termP2 :: Parser Exp
termP2 = choice [parens expP2, numberP]

expP :: Parser Exp
expP = makeExprParser termP operatorTable

expP2 :: Parser Exp
expP2 = makeExprParser termP2 operatorTable2

operatorTable :: [[Operator Parser Exp]]
operatorTable = [[binary "*" Mult, binary "+" Add]]

operatorTable2 :: [[Operator Parser Exp]]
operatorTable2 = [[binary "+" Add], [binary "*" Mult]]

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

readInput :: MonadIO f => FilePath -> f [Exp]
readInput path = mapMaybe parseExp . lines <$> readFileText path

readInput2 :: MonadIO f => FilePath -> f [Exp]
readInput2 path = mapMaybe parseExp2 . lines <$> readFileText path

-----------------------------------

-- converge :: Eq t => (t -> t) -> t -> t
-- converge f x = let res = f x in if res == x then res else converge f res

-- applyTimes :: (c -> c) -> Int -> c -> c
-- applyTimes f n = (!! n) . iterate f

-- count :: (Ord k, Foldable t) => t k -> k -> Int
-- count xs x = fromMaybe 0 (c IMap.!? x)
--   where
--     c = IMap.fromListWith (+) . fmap (,1) . toList $ xs

-- readInt :: Text -> Int
-- readInt = fromJust . readMaybe . toString

-- head' :: [c] -> c
-- head' = fromJust . viaNonEmpty head

-- headMay :: [b] -> Maybe b
-- headMay = viaNonEmpty head

-- tail' :: [a] -> [a]

-- search :: Eq a => Vector (Vector a) -> Vector (Vector a)
-- search vs
--  | isSolution vs = vs
--  | otherwise =
--    let candidateIndex = V.minIndexBy (comparing V.length) vs
--     in search . eliminate $ assign vs candidateIndex

-- assign :: Vector (Vector a) -> Int -> Vector (Vector a)
-- assign vs i = vs V.// [(i, V.singleton (V.head (vs V.! i)))]

-- solutionAsList :: Vector (Vector a) -> [a]
-- solutionAsList = toList . V.map V.head

-- isSolution :: Vector (Vector a) -> Bool
-- isSolution = all (\v -> V.length v == 1)

-- eliminate :: Eq a => Vector (Vector a) -> Vector (Vector a)
-- eliminate v = foldl' eliminateAt v [0 .. V.length v - 1]

-- eliminateAt :: Eq a => Vector (Vector a) -> Int -> Vector (Vector a)
-- eliminateAt vs i
--   | V.length cur == 1 =
--     let val = V.head cur
--      in V.imap (\j v -> if i /= j then V.filter (/= val) v else v) vs
--   | otherwise = vs
--   where
--     cur = vs V.! i
