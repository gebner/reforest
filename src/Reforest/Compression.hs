module Reforest.Compression where
import Reforest.Grammar
import Control.Arrow
import Control.Monad
import Data.List
import Data.Function
import Data.Maybe
import qualified Data.Set as Set

data Ngram = Digram Sym Int Sym | RigidTrigram Sym Int Int
            deriving (Eq, Ord, Show)

listDigrams' :: Term -> [Ngram] -> [Ngram]
listDigrams' (App s xs) = localDigrams 0 xs
  where
    localDigrams _ [] = id
    localDigrams i (y@(App (Bnd _) _) : ys) = listDigrams' y . localDigrams (i+1) ys
    localDigrams i (y@(App r _) : ys) = listDigrams' y . (Digram s i r :) . localDigrams (i+1) ys

listDigrams :: Grammar -> [Ngram]
listDigrams = foldr (listDigrams' . rhs) []

listRigidTrigrams' :: Term -> [Ngram] -> [Ngram]
listRigidTrigrams' (App s xs) rest =
    [ RigidTrigram s i j |
        (i, App r1 _) <- zip [0..] xs,
        (j, App r2 _) <- zip [0..] xs,
        i < j, r1 == r2
    ] ++ foldr listRigidTrigrams' rest xs

listRigidTrigrams :: Grammar -> [Ngram]
listRigidTrigrams = foldr (listRigidTrigrams' . rhs) []

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = map (head &&& length) . group . sort

sortedNgrams :: Grammar -> [(Ngram, Int)]
sortedNgrams g = sortBy (flip compare `on` snd) . frequencies $ listDigrams g ++ listRigidTrigrams g

maxNonTermNo :: Grammar -> Int
maxNonTermNo = maximum . map (\(Prod (NT i _) _) -> i)

abbreviateNgram :: Ngram -> Grammar -> Grammar
abbreviateNgram d@(Digram s i r) g = newProd : map abbrProd g
    where
      newNT = NT (maxNonTermNo g + 1) (arity s + arity r - 1)
      newProd = Prod newNT (App s sArgs)
      sArgs = [ App (Bnd j) [] | j <- [0..i-1] ] ++ [App r rArgs] ++ [ App (Bnd (j+arity r-1)) [] | j <- [i+1..arity s-1] ]
      rArgs = [ App (Bnd (i+j)) [] | j <- [0..arity r-1] ]
      abbrProd (Prod a t) = Prod a (abbreviateNgramBy d newNT t)
abbreviateNgram ngram@(RigidTrigram s i j) g = newProd : map abbrProd g
    where
      newNT = NT (maxNonTermNo g + 1) (arity s - 1)
      newProd = Prod newNT (App s sArgs)
      sArgs = [ App (Bnd k) [] | k <- [0..j-1] ] ++ [App (Bnd i) []] ++ [ App (Bnd (k-1)) [] | k <- [j+1..arity s-1] ]
      abbrProd (Prod a t) = Prod a (abbreviateNgramBy ngram newNT t)

abbreviateNgramBy :: Ngram -> NonTerm -> Term -> Term
abbreviateNgramBy d@(Digram s i r) nt (App f xs)
  | f == s && rootSym (xs !! i) == r = abbreviateNgramBy d nt $
      App (Var nt) $ [ xs !! j | j <- [0..i-1] ] ++ args (xs !! i) ++ [ xs !! j | j <- [i+1..arity s-1] ]
  | otherwise = App f (map (abbreviateNgramBy d nt) xs)
abbreviateNgramBy d@(RigidTrigram s i j) nt (App f xs)
  | f == s && xs !! i == xs !! j = abbreviateNgramBy d nt $
      App (Var nt) [ xs !! k | k <- [0..arity s-1], k /= j ]
  | otherwise = App f (map (abbreviateNgramBy d nt) xs)

abbreviateMostCommonDigram :: Grammar -> Grammar
abbreviateMostCommonDigram g = abbreviateNgram (fst $ head $ sortedNgrams g) g

ngramCompression :: Grammar -> Grammar
ngramCompression g
      | f > 1 = ngramCompression (abbreviateNgram dg g)
      | otherwise = g
    where ((dg, f) : _) = sortedNgrams g

extractSimpleA0Rule :: Production -> Maybe (Sym, Sym)
extractSimpleA0Rule p
  | lhs p == NT 0 0 = extractSimpleARule p
  | otherwise = Nothing

extractSimpleARule :: Production -> Maybe (Sym, Sym)
extractSimpleARule (Prod (NT _ 0) (App s [App r []])) = Just (s, r)
extractSimpleARule _ = Nothing

splitR :: NonTerm -> ([Sym], [Sym]) -> Grammar -> Grammar
splitR a (ss, rs) g = newProds ++ filter (not . isSubsumed) g
  where
    isSubsumed p@(Prod b _) =
      case extractSimpleARule p of
        Just (s, r) -> a == b && s `elem` ss && r `elem` rs
        Nothing -> False
    newNT = NT (maxNonTermNo g + 1) 0
    newProds = [ Prod a (App s [App (Var newNT) []]) | s <- ss ]
            ++ [ Prod newNT (App r []) | r <- rs ]

naiveSplit :: NonTerm -> Grammar -> ([Sym],[Sym])
naiveSplit a g = (nub' (map fst edges), nub' (map snd edges))
    where edges = mapMaybe extractSimpleARule $ prods a g

findNaiveSplits :: Grammar -> [(NonTerm, [Sym], [Sym])]
findNaiveSplits g = do
    (a,f) <- frequencies (map lhs g)
    guard $ f > 1
    let (rr,ss) = naiveSplit a g
    guard $ length rr > 1
    guard $ length ss > 1
    return (a, rr, ss)

refs :: Term -> [NonTerm]
refs (App (Var n) xs) = n : concatMap refs xs
refs (App _ xs) = concatMap refs xs

singleRefd :: Grammar -> [NonTerm]
singleRefd = map fst . filter ((==1) . snd) . frequencies . concatMap (refs . rhs)

simpl :: Grammar -> Grammar
simpl g = foldr elimDef g defbls
    where
      defbls = map lhs $ mapMaybe (unambDef g) (singleRefd g)

simplFull :: Grammar -> Grammar
simplFull g = foldr elimDef g defbls
    where
      defbls = map lhs $ mapMaybe (unambDef g . lhs) g

tryIntroNT :: Grammar -> Maybe Grammar
tryIntroNT g =
    case findNaiveSplits g of
      [] -> Nothing
      splits -> Just $ foldr (\(a, rr,ss) -> splitR a (rr,ss)) g splits

nTryIntroNT :: Grammar -> Grammar
nTryIntroNT g =
    case tryIntroNT g of
      Just g' -> nTryIntroNT $ simpl $ ngramCompression g'
      Nothing -> g

langToGrammar :: [Term] -> Grammar
langToGrammar = map (Prod (NT 0 0))

findTratGrammar :: [Term] -> Grammar
findTratGrammar = simplFull . nTryIntroNT . ngramCompression . langToGrammar

subterms :: Term -> [Term]
subterms t@(App _ xs) = t : concatMap subterms xs

minimalDAG :: [Term] -> Grammar
minimalDAG lang = simpl $ (abbrevProds ++) $ map (Prod (NT 0 0) . abbrev) lang
    where
      subtermAbbrevs = zip (nub' $ concatMap subterms lang) [1..] :: [(Term, Int)]
      abbrev t = App (Var (NT i 0)) [] where Just i = lookup t subtermAbbrevs
      abbrevProds = map (\(App f xs, i) -> Prod (NT i 0) (App f (map abbrev xs))) subtermAbbrevs
