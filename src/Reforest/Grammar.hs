module Reforest.Grammar where
import Data.List
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

nonTerms :: Grammar -> [NonTerm]
nonTerms = nub' . map lhs

projections :: Grammar -> [Grammar]
projections g = sequence [ prods a g | a <- nonTerms g ]

unambDef :: Grammar -> NonTerm -> Maybe Production
unambDef g a = case prods a g of
                 [p] -> Just p
                 _ -> Nothing

subst :: Production -> Term -> Term
subst p@(Prod a t) (App (Var a') xs)
  | a == a' = subst' (map (subst p) xs) t
subst p (App f xs) = App f (map (subst p) xs)

subst' :: [Term] -> Term -> Term
subst' as (App (Bnd i) []) = as !! i
subst' as (App f xs) = App f (map (subst' as) xs)

elimDef :: NonTerm -> Grammar -> Grammar
elimDef n g = map elimDef' (filter ((/= n) . lhs) g)
  where elimDef' (Prod a t) = Prod a (subst p t)
        Just p = unambDef g n

genLang'' :: NonTerm -> Grammar -> [Term]
genLang'' a g = map rhs $ prods a $ foldr elimDef g (nonTerms g \\ [a])

genLang' :: NonTerm -> Grammar -> [Term]
genLang' a g = nub' (projections g >>= genLang'' a)

genLang :: Grammar -> [Term]
genLang = genLang' (NT 0 0)
