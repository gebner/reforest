import Reforest
import Data.List
import Data.Either

-- exampleLang :: [Term]
-- exampleLang = [f (g c c) (g c c), g (f c c) (f c c), f (g d d) (g d d), g (f d d) (f d d)]
--   where
--     f x y = App (Con "f" 2) [x,y]
--     g x y = App (Con "g" 2) [x,y]
--     c = App (Con "c" 0) []
--     d = App (Con "d" 0) []

exampleLang :: [Term]
exampleLang = concatMap (\i -> map (r i) sq) [0..9]
  where
    sq = take 50 $ iterate f c
    f x = App (Con "f" 1) [x]
    r i x = App (Con ("r" ++ show (i :: Int)) 1) [x]
    c = App (Con "c" 0) []

exampleLang2 :: [Term]
exampleLang2 = l 5
  where
    f x y = App (Con "f" 2) [x,y]
    g x y = App (Con "g" 2) [x,y]
    c = App (Con "c" 0) []
    d = App (Con "d" 0) []

    l :: Int -> [Term]
    l 0 = [c,d]
    l i = do { t <- l (i-1); [f t t, g t t] }

exampleLang3 :: [Term]
exampleLang3 = l n
  where
    n = 5

    f x y = App (Con "h" 1) [App (Con "f" 2) [x,y]]
    g x y = App (Con "g" 2) [x,y]
    c = App (Con "c" 0) []
    d = App (Con "d" 0) []

    numeral 0 = App (Con "z" 0) []
    numeral i = App (Con "s" 1) [numeral (i-1)]

    l :: Int -> [Term]
    l 0 = [c,d]
    l i = do { t <- l (i-1); [f t (numeral (n-i)), g t (numeral (n-i))] }

exampleTermSet = rights $ map parseTerm ["tuple5(identity,a,b,a)","tuple5(a,c,multiply(a,c),multiply(b,c))","tuple4(a)","tuple4(b)","tuple4(multiply(b,c))","tuple3(identity,b,a,c,multiply(b,c),multiply(b,c))","tuple2(a,b,identity,a)","tuple2(b,a,identity,b)","tuple1(b,c)","tuple1(a,c))"]

termsFromFile :: FilePath -> IO [Term]
termsFromFile fn = do
  cont <- readFile fn
  let Right terms = mapM parseTerm (lines cont)
  return terms

main :: IO ()
main = mapM_ print $ sort $ findTratGrammar exampleLang2
