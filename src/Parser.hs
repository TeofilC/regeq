module Parser where
-- import Control.Applicative
import Text.Parsec hiding (Empty)
import Text.Parsec.Char
import Data.List ((\\))
import Data.Functor
import Regex

-- allChar is every ASCII character
allChar :: [Char]
allChar = enumFromTo '!' (toEnum 126)

type Parser = Parsec String ()

sym :: Parser Char
sym = do
  char '\''
  c <- anyChar
  char '\''
  return $ c

charset :: Parser (Regex Char)
charset = do
  char '['
  neg <- optionMaybe (char '^')
  chars <- many1 sym
  char ']'
  case neg of
    Just _ -> return $ foldr1 Union $ map Sym $ (allChar \\ chars)
    Nothing -> return $ foldr1 Union $ map Sym chars

regexp :: Parser (Regex Char)
regexp =  try (Union <$> factor <* char '|' <*> regexp)
      <|> factor

factor =   try (Concat <$> term <*> factor)
       <|> term

term :: Parser (Regex Char)
term = do
  b <- base
  try (char '*' >> return (Star b))
   <|> try (char '+' >> return (Concat b (Star b)))
   <|> return b

base :: Parser (Regex Char)
base =
          Sym <$> sym
      <|> charset
      <|> (char '(' >> regexp <* char ')')

parseRegex = parse (regexp <* eof) ""
