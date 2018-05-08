import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "How a \"jeux\" is played in a tennis game "$ do
    it "returns 0 - 0 when there was no winning strike from each part of the players" $
      score [] (Zero, Zero) (0, 0) `shouldBe` ((Zero, Zero), (0, 0))

    it "returns 15-0 when player A makes a winning strike" $
      score [A] (Zero, Zero) (0, 0) `shouldBe` ((Quinze, Zero), (0, 0))

    it "returns 30-0 when player A makes two consecutive winning strikes" $
      score [A, A] (Zero, Zero) (0, 0) `shouldBe` ((Trente, Zero), (0, 0))

    it "returns 40-0 when player A makes three consecutive winning strikes" $
      score [A, A, A] (Zero, Zero) (0, 0) `shouldBe` ((Quarante, Zero), (0, 0))

    it "return 40-15 when Player A makes three winning strikes and Player B makes one winning strike" $
      score [A, A, B, A] (Zero, Zero) (0, 0) `shouldBe` ((Quarante, Quinze), (0, 0))


    {-{-it "is deuce when Player A and B makes the same number of winning strikes" $-}-}
      {-{-score [A, A, B, A] (Zero, Zero) `shouldBe` (Quarante, Quinze)-}-}

    {-it "gives one point to player A if he makes 4 winning hits  in the game"-}
      {-score [A, A, A, A] (Zero, Zero) (Int, Int) `shouldBe` [(Zero, Zero), (1, 0)]-}

  describe "How a \"set\" is played in a tennis game "$ do
    it "returns 1 - 0 when player A wons a \"jeux\"" $
      score [A, A, A, A] (Zero, Zero) (0, 0)`shouldBe` ((Zero, Zero), (1, 0))

    it "returns 0 - 1 when player B wons a \"jeux\"" $
      score [B, B, B, B] (Zero, Zero) (0, 0)`shouldBe` ((Zero, Zero), (0, 1))

    it "returns 2 - 0 when player A wons 2 \"jeux\" in a row" $
      score [A, A, A, A, A, A, A, A] (Zero, Zero) (0, 0)`shouldBe` ((Zero, Zero), (2, 0))

    it "returns 6 - 0 when player A wons 6 \"jeux\" in a row" $
      score [
      A, A, A, A,
      A, A, A, A,
      A, A, A, A,
      A, A, A, A,
      A, A, A, A,
      A, A, A, A] (Zero, Zero) (0, 0)`shouldBe` ((Zero, Zero), (6, 0))

    it "returns 6 - 3 when player A wons 6 \"jeux\" in player B 3 \"jeux\"" $
      score [
      A, A, A, A,
      A, A, A, A,
      B, B, B, B,
      A, A, A, A,
      A, A, A, A,
      B, B, B, B,
      A, A, A, A,
      B, B, B, B,
      A, A, A, A] (Zero, Zero) (0, 0)`shouldBe` ((Zero, Zero), (6, 3))

  describe "How a \"game\" is played in a tennis game "$ do
    it "returns 1 - 0 when player A wons a \"jeux\"" $
      score [A, A, A, A] (Zero, Zero) (0, 0)`shouldBe` ((Zero, Zero), (1, 0))


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
score :: [Player] -> (Point, Point) -> (Integer, Integer)-> ((Point, Point), (Integer, Integer))
score (A:tail) (Quarante, pointsB) (jeuxA, jeuxB) = score tail (Zero, Zero) (jeuxA + 1, jeuxB)
score (B:tail) (pointsA, Quarante) (jeuxA, jeuxB) = score tail (Zero, Zero) (jeuxA, jeuxB + 1)

score (A:tail) (pointsA, pointsB) (jeuxA, jeuxB) = score tail (succ pointsA, pointsB) (jeuxA, jeuxB)
score (B:tail) (pointsA, pointsB) (jeuxA, jeuxB) = score tail (pointsA, succ pointsB) (jeuxA, jeuxB)

score [] (pointsA, pointsB) (jeuxA, jeuxB) = ((pointsA, pointsB), (jeuxA, jeuxB))
