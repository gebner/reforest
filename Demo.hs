import Reforest
import Data.List

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
    sq = take 100 $ iterate f c
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

main :: IO ()
main = mapM_ print $ sort $ findTratGrammar exampleLang2
