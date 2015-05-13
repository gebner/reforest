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
