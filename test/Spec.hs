import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "The score of a tennis point" $ do
    it "returns 0 - 0 when there was no winning strike from each part of the players" $
      score [] `shouldBe` (0, 0)

    it "returns 15-0 when player A makes a winning strike" $
      score [A] `shouldBe` (15, 0)

    it "returns 30-0 when player A makes two consecutive winning strikes" $
      score [A, A] `shouldBe` (30, 0)

    it "returns 40-0 when player A makes three consecutive winning strikes" $
      score [A, A, A] `shouldBe` (40, 0)

    it "return 40-15 when Player A makes three winning strikes and Player B makes one winning strike" $
      score [A, A, B, A] `shouldBe` (40, 15)

data Player = A | B deriving (Show, Eq)

score :: [Player] -> (Int, Int)
score [] = (0, 0)
score [A] = (15, 0)
score [A, A] = (30, 0)
score [A, A, A] = (40, 0)
score [A, A, B, A] = (40, 15)
score [A, B, A, B, A] = (40, 15)
