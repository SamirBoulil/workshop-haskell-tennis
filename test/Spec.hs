import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "The score of a tennis point" $ do
    it "returns 0 - 0 when there was no winning strike from each part of the players" $
      score [] (Zero, Zero) `shouldBe` (Zero, Zero)

    it "returns 15-0 when player A makes a winning strike" $
      score [A] (Zero, Zero) `shouldBe` (Quinze, Zero)

    it "returns 30-0 when player A makes two consecutive winning strikes" $
      score [A, A] (Zero, Zero) `shouldBe` (Trente, Zero)

    it "returns 40-0 when player A makes three consecutive winning strikes" $
      score [A, A, A] (Zero, Zero) `shouldBe` (Quarante, Zero)

    it "return 40-15 when Player A makes three winning strikes and Player B makes one winning strike" $
      score [A, A, B, A] (Zero, Zero) `shouldBe` (Quarante, Quinze)

data Player = A | B deriving (Show, Eq)

data Point = Zero | Quinze | Trente | Quarante deriving (Show, Eq, Enum)

{-Pattern matching-}
{-score :: [Player] -> (Int, Int)-}
{-score [] = (0, 0)-}
{-score [A] = (15, 0)-}
{-score [A, A] = (30, 0)-}
{-score [A, A, A] = (40, 0)-}
{-score [A, A, B, A] = (40, 15)-}
{-score [A, B, A, B, A] = (40, 15)-}

{-Recursion + Pattern matching-}
score :: [Player] -> (Point, Point) -> (Point, Point)
score (A:tail) (scoreA, scoreB) = score tail (succ scoreA, scoreB)
score (B:tail) (scoreA, scoreB) = score tail (scoreA, succ scoreB)
score [] (scoreA, scoreB) = (scoreA, scoreB)
