module Reforest.Parser where
import Reforest.Grammar
import Text.Parsec
import Text.Parsec.String
import Control.Monad
import Data.Char

data ParsedSym = PNT Int | PCon String | PBnd Int
               deriving (Eq, Ord, Show)

psym2sym :: ParsedSym -> Int -> Sym
psym2sym (PNT i) n = Var (NT i n)
psym2sym (PCon f) n = Con f n
psym2sym (PBnd i) 0 = Bnd i
psym2sym (PBnd _) _ = error "non-nullary bound variable"

nt :: Parser ParsedSym
nt = do
    _ <- char 'A'
    num <- many1 digit
    return $ PNT (read num)

bnd :: Parser ParsedSym
bnd = do
    _ <- char 'x'
    num <- many1 digit
    return $ PBnd (read num)

con :: Parser ParsedSym
con = PCon `liftM` many (noneOf "(,)")

psym :: Parser ParsedSym
psym = (try nt <|> try bnd <|> con) <* spaces

arguments :: Parser [Term]
arguments = between (char '(' <* spaces) (char ')' <* spaces) $ sepBy term (char ',' <* spaces)

term :: Parser Term
term = do
    f <- psym
    xs <- try arguments <|> return []
    return $ App (psym2sym f (length xs)) xs

parseTerm :: String -> Either ParseError Term
parseTerm = parse onlyTerm "parseTerm"
  where onlyTerm = do { t <- term; eof; return t }
