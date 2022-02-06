import Prelude hiding (init)
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Sauna
import Sauna.Data.State
import Sauna.Data.Letter
import Sauna.Data.Word
import Sauna.Data.Color
import Sauna.Data.Response
import Sauna.Data.Alphabet
import Data.Quintuple
import Data.List ((\\))



main :: IO ()
main = hspec $ do
  describe "Sauna.options" $ do
    it "return everything for empty state" $ do
      options init `shouldBe` pure fullAlphabet
    it "handles solo letters correcly" $ do
      options
        (State [(Word (Quintuple (A,B,C,D,E)),Response (Quintuple (Green,Yellow,Black,Black,Black)))])
        `shouldBe` Quintuple (
          Alphabet [A],
          Alphabet ([A ..] \\ [B,C,D,E]),
          Alphabet ([A ..] \\ [C,D,E]),
          Alphabet ([A ..] \\ [C,D,E]),
          Alphabet ([A ..] \\ [C,D,E])
        )
    it "handles double letters correcrly" $ do
      options
        (State [(Word (Quintuple (A,A,B,B,B)),Response (Quintuple (Yellow,Black,Black,Black,Black)))])
        `shouldBe` Quintuple (
          Alphabet ([A ..] \\ [A,B]),
          Alphabet ([A ..] \\ [A,B]),
          Alphabet ([A ..] \\ [B]),
          Alphabet ([A ..] \\ [B]),
          Alphabet ([A ..] \\ [B])
        )
    it "handles triple letters correcrly" $ do
      options
        (State [(Word (Quintuple (A,A,A,B,B)),Response (Quintuple (Yellow,Black,Black,Black,Black)))])
        `shouldBe` Quintuple (
          Alphabet ([A ..] \\ [A,B]),
          Alphabet ([A ..] \\ [A,B]),
          Alphabet ([A ..] \\ [A,B]),
          Alphabet ([A ..] \\ [B]),
          Alphabet ([A ..] \\ [B])
        )
    it "handles quadruple letters correcrly" $ do
      options
        (State [(Word (Quintuple (A,A,A,A,B)),Response (Quintuple (Yellow,Black,Black,Black,Black)))])
        `shouldBe` Quintuple (
          Alphabet ([A ..] \\ [A,B]),
          Alphabet ([A ..] \\ [A,B]),
          Alphabet ([A ..] \\ [A,B]),
          Alphabet ([A ..] \\ [A,B]),
          Alphabet ([A ..] \\ [B])
        )
