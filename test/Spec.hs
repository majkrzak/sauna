import Prelude hiding (init)
import Test.Hspec
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
      options initialize `shouldBe` pure fullAlphabet
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
  describe "Sauna.check" $ do
    it "return all Black for diferent words" $ do
      check (read "ABCDE") (read "FGHIJ") `shouldBe` (read "BBBBB" :: Response)
    it "return all Green for same words" $ do
      check (read "ABCDE") (read "ABCDE") `shouldBe` (read "GGGGG" :: Response)
    it "return Yellow for the signle same letter on diferent positions" $ do
      check (read "ABCDE") (read "BGHIJ") `shouldBe` (read "YBBBB" :: Response)
      check (read "ABCDE") (read "FCHIJ") `shouldBe` (read "BYBBB" :: Response)
      check (read "ABCDE") (read "FGDIJ") `shouldBe` (read "BBYBB" :: Response)
      check (read "ABCDE") (read "FGHEJ") `shouldBe` (read "BBBYB" :: Response)
      check (read "ABCDE") (read "FGHIA") `shouldBe` (read "BBBBY" :: Response)
    it "return Yellow only for one occurence of the same letter on diferent positions" $ do
      check (read "ABCDE") (read "BGBBB") `shouldBe` (read "YBBBB" :: Response)
      check (read "ABCDE") (read "FCHCC") `shouldBe` (read "BYBBB" :: Response)
      check (read "ABCDE") (read "FGDID") `shouldBe` (read "BBYBB" :: Response)
    it "return Green for first occurence and ignores next" $ do
     check (read "ABCDE") (read "AAAAA") `shouldBe` (read "GBBBB" :: Response)
    it "return Green for first occurence and ignores previous" $ do
     check (read "ABCDE") (read "BBBBB") `shouldBe` (read "BGBBB" :: Response)
    it "return Green for first and Yellow for next occurence" $ do
      check (read "ABCCE") (read "ZZCZC") `shouldBe` (read "BBGBY" :: Response)
    it "return Green for first and Yellow for previous occurence" $ do
      check (read "ABCCE") (read "ZCCZC") `shouldBe` (read "BYGBB" :: Response)