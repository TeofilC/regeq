module Parser where
-- import Control.Applicative
import Text.Parsec
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
    Just _ -> return $ binfold Union $ map Sym $ (allChar \\ chars)
    Nothing -> return $ binfold Union $ map Sym chars

binfold f [x] = x
binfold f xs  = binfold f $ bf f xs
bf f []       = []
bf f [x]      = [x]
bf f (x:y:xs) = (f x y):bf f xs

regexp :: Parser (Regex Char)
regexp =
          try (Union <$> term <* char '|' <*> regexp)
      <|> try (Concat <$> term <*> regexp)
      <|> term

term :: Parser (Regex Char)
term =
           try (Star <$> base <* char '*')
       <|> base

base :: Parser (Regex Char)
base =
          Sym <$> sym
      <|> charset
      <|> (char '(' >> regexp <* char ')')

parseRegex = parse (regexp <* eof) ""
