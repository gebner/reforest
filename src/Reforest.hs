module Reforest where
import Control.Arrow
import Control.Monad
import Data.List
import Data.Function
import Data.Maybe
import qualified Data.Set as Set

nub' :: Ord a => [a] -> [a]
nub' = Set.toList . Set.fromList

data NonTerm = NT Int Int
             deriving (Eq, Ord)

instance Show NonTerm where
    show (NT i _) = "A" ++ show i

data Sym = Con String Int | Var NonTerm | Bnd Int
         deriving (Eq, Ord)

arity :: Sym -> Int
arity (Con _ k) = k
arity (Var (NT _ k)) = k
arity (Bnd _) = 0

instance Show Sym where
    show (Con f _) = f
    show (Var a) = show a
    show (Bnd i) = "x" ++ show i

data Term = App Sym [Term]
          deriving (Eq, Ord)

rootSym :: Term -> Sym
rootSym (App f _) = f

args :: Term -> [Term]
args (App _ xs) = xs

instance Show Term where
    show (App s []) = show s
    show (App s xs) = show s ++ "(" ++ intercalate "," (map show xs) ++ ")"

data Production = Prod NonTerm Term
                deriving (Eq, Ord)

lhs :: Production -> NonTerm
rhs :: Production -> Term
lhs (Prod a _) = a
rhs (Prod _ t) = t

instance Show Production where
    show (Prod a@(NT _ k) t) = show (App (Var a) [App (Bnd i) [] | i <- [0..k-1]]) ++ "->" ++ show t

type Grammar = [Production]

prods :: NonTerm -> Grammar -> Grammar
prods a = filter ((== a) . lhs)

data Digram = Digram Sym Int Sym
            deriving (Eq, Ord, Show)

listDigrams' :: Term -> [Digram] -> [Digram]
listDigrams' (App s xs) = localDigrams 0 xs
  where
    localDigrams _ [] = id
    localDigrams i (y@(App (Bnd _) _) : ys) = listDigrams' y . localDigrams (i+1) ys
    localDigrams i (y@(App r _) : ys) = listDigrams' y . (Digram s i r :) . localDigrams (i+1) ys

listDigrams :: Grammar -> [Digram]
listDigrams = foldr (listDigrams' . rhs) []

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = map (head &&& length) . group . sort

sortedDigrams :: Grammar -> [(Digram, Int)]
sortedDigrams = sortBy (flip compare `on` snd) . frequencies . listDigrams

maxNonTermNo :: Grammar -> Int
maxNonTermNo = maximum . map (\(Prod (NT i _) _) -> i)

abbreviateDigram :: Digram -> Grammar -> Grammar
abbreviateDigram d@(Digram s i r) g = newProd : map abbrProd g
    where
      newNT = NT (maxNonTermNo g + 1) (arity s + arity r - 1)
      newProd = Prod newNT (App s sArgs)
      sArgs = [ App (Bnd j) [] | j <- [0..i-1] ] ++ [App r rArgs] ++ [ App (Bnd (j+arity r-1)) [] | j <- [i+1..arity s-1] ]
      rArgs = [ App (Bnd (i+j)) [] | j <- [0..arity r-1] ]
      abbrProd (Prod a t) = Prod a (abbreviateDigramBy d newNT t)

abbreviateDigramBy :: Digram -> NonTerm -> Term -> Term
abbreviateDigramBy d@(Digram s i r) nt (App f xs)
  | f == s && rootSym (xs !! i) == r = abbreviateDigramBy d nt $
      App (Var nt) $ [ xs !! j | j <- [0..i-1] ] ++ args (xs !! i) ++ [ xs !! j | j <- [i+1..arity s-1] ]
  | otherwise = App f (map (abbreviateDigramBy d nt) xs)

abbreviateMostCommonDigram :: Grammar -> Grammar
abbreviateMostCommonDigram g = abbreviateDigram (fst $ head $ sortedDigrams g) g

digramCompression :: Grammar -> Grammar
digramCompression g = if f == 1 then g else digramCompression (abbreviateDigram dg g)
    where ((dg, f) : _) = sortedDigrams g

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

unambDef :: Grammar -> NonTerm -> Maybe Production
unambDef g a = case prods a g of
                 [p] -> Just p
                 _ -> Nothing

elimDef :: NonTerm -> Grammar -> Grammar
elimDef n g = map elimDef' (filter ((/= n) . lhs) g)
  where elimDef' (Prod a t) = Prod a (subst p t)
        Just p = unambDef g n

subst :: Production -> Term -> Term
subst p@(Prod a t) (App (Var a') xs)
  | a == a' = subst' (map (subst p) xs) t
subst p (App f xs) = App f (map (subst p) xs)

subst' :: [Term] -> Term -> Term
subst' as (App (Bnd i) []) = as !! i
subst' as (App f xs) = App f (map (subst' as) xs)

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
      Just g' -> nTryIntroNT $ simpl $ digramCompression g'
      Nothing -> g

langToGrammar :: [Term] -> Grammar
langToGrammar = map (Prod (NT 0 0))

findTratGrammar :: [Term] -> Grammar
findTratGrammar = simplFull . nTryIntroNT . digramCompression . langToGrammar

subterms :: Term -> [Term]
subterms t@(App _ xs) = t : concatMap subterms xs

minimalDAG :: [Term] -> Grammar
minimalDAG lang = simpl $ (abbrevProds ++) $ map (Prod (NT 0 0) . abbrev) lang
    where
      subtermAbbrevs = zip (nub' $ concatMap subterms lang) [1..] :: [(Term, Int)]
      abbrev t = App (Var (NT i 0)) [] where Just i = lookup t subtermAbbrevs
      abbrevProds = map (\(App f xs, i) -> Prod (NT i 0) (App f (map abbrev xs))) subtermAbbrevs
