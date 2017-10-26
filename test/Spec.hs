module Main where
import Test.Hspec
import Parser
import Regex
import NFA
import DFA
import Util


main :: IO ()
main = hspec $ do
  describe "Util.eq" $ do
    it "shows x = x" $ do
      eq "'a'" "'a'" `shouldBe` Nothing
      eq "('a'|'b')|'b'*"  "('a'|'b')|'b'*" `shouldBe` Nothing

    it "gives the shortest uncommon word in case of inequality" $ do
      eq "'a'" "'b''b'" `shouldBe` (Just "a")

    it "confirms Kozen's axioms" $ do
      eq "'E'|('F'|'G')" "('E'|'F')|'G'" `shouldBe` Nothing
      eq "'E'|'F'" "'F'|'E'" `shouldBe` Nothing
      -- eq "'E'|" "'E'" `shouldBe` Nothing
      eq "'E'|'E'" "'E'" `shouldBe` Nothing
      eq "('E''F')'G'" "'E'('F''G')" `shouldBe` Nothing
      eq "'E'('F'|'G')" "'E''F'|'E''G'" `shouldBe` Nothing
      eq "('E'|'F')'G'" "'E''G'|'F''G'" `shouldBe` Nothing

    it "shows equivalent regular expressions are in fact equivalent" $ do
      eq "'a''b'('a'|'b'*'c')*'b'+'a'" "'a''b'('a'|'c')*'b'('b'|'c'('a'|'c')*'b')*'a'" `shouldBe` Nothing
      eq "('a'|'b')*" "'a'*('b''a'*)*" `shouldBe` Nothing
