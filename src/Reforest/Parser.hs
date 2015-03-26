module Reforest.Parser where
import Reforest
import Text.Parsec
import Text.Parsec.String
import Control.Monad

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
    num <- many digit
    return $ PNT (read num)

bnd :: Parser ParsedSym
bnd = do
    _ <- char 'x'
    num <- many digit
    return $ PBnd (read num)

con :: Parser ParsedSym
con = PCon `liftM` many alphaNum

psym :: Parser ParsedSym
psym = try nt <|> try bnd <|> con

arguments :: Parser [Term]
arguments = between (char '(') (char ')') $ sepBy term (char ',')

term :: Parser Term
term = do
    f <- psym
    xs <- try arguments <|> return []
    return $ App (psym2sym f (length xs)) xs

parseTerm :: String -> Either ParseError Term
parseTerm = parse term "parseTerm"
