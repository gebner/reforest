module Reforest.CompressionSpec where
import Test.Hspec
import Reforest

spec :: Spec
spec = do
    describe "abbreviateNgram" $ do
      let Right t1 = parseTerm "f(c,c)"
      let Right t2 = parseTerm "f(c,d)"
      let g = langToGrammar [t1,t2]
      let g' = abbreviateNgram (Digram (Con "f" 2) 0 (Con "c" 0)) g
      it "adds a new production" $ length g' `shouldBe` length g + 1
      it "adds a new non-terminal" $ length (nub' (map lhs g')) == 2
