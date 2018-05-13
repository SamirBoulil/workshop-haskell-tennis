import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "How a \"jeux\" is played in a tennis game "$ do
    it "returns 0 - 0 when there was no winning strike from each part of the players" $
      scoreOfGame [] `shouldBe` Match (Zero, Zero) (Set 0 0) (Set 0 0)


    it "returns 15-0 when player A makes a winning strike" $
      scoreOfGame [A] `shouldBe` Match (Quinze, Zero) (Set 0 0) (Set 0 0)


    it "returns 30-0 when player A makes two consecutive winning strikes" $
      scoreOfGame [A, A] `shouldBe` Match (Trente, Zero) (Set 0 0) (Set 0 0)


    it "returns 40-0 when player A makes three consecutive winning strikes" $
      scoreOfGame [A, A, A] `shouldBe` Match (Quarante, Zero) (Set 0 0) (Set 0 0)


    it "return 40-15 when Player A makes three winning strikes and Player B makes one winning strike" $
      scoreOfGame [A, A, B, A] `shouldBe` Match (Quarante, Quinze) (Set 0 0) (Set 0 0)


    it "return 40-40" $
      scoreOfGame [A, A, A, B, B, B] `shouldBe` Match (Quarante, Quarante) (Set 0 0) (Set 0 0)


    it "return Avantage A" $
      scoreOfGame [A, A, A, B, B, B, A] `shouldBe` Match (Avantage, Quarante) (Set 0 0) (Set 0 0)


    it "return Avantage A after a few deuces" $
      scoreOfGame [A, A, A, B, B, B, A, B, B, A, A] `shouldBe` Match (Avantage, Quarante) (Set 0 0) (Set 0 0)


    it "return A wins the point after avantage" $
      scoreOfGame [A, A, A, B, B, B, A, A] `shouldBe` Match (Zero, Zero) (Set 1 0) (Set 0 0)


    it "gives one point to player A if he makes 4 winning hits  in the game" $
      scoreOfGame [A, A, A, A] `shouldBe` Match (Zero, Zero) (Set 1 0) (Set 0 0)


  describe "How a \"set\" is played in a tennis game "$ do
    it "returns 0 - 1 when player B wons a \"jeux\"" $
      scoreOfGame [B, B, B, B] `shouldBe` Match (Zero, Zero) (Set 0 1) (Set 0 0)


    it "returns 2 - 0 when player A wons 2 \"jeux\" in a row" $
      scoreOfGame [A, A, A, A, A, A, A, A] `shouldBe` Match (Zero, Zero) (Set 2 0) (Set 0 0)


    it "returns 6 - 0 when player A wons 6 \"jeux\" in a row" $
      scoreOfGame [
      A, A, A, A,
      A, A, A, A,
      A, A, A, A,
      A, A, A, A,
      A, A, A, A,
      A, A, A, A] `shouldBe` Match (Zero, Zero) (Set 6 0) (Set 0 0)


    it "returns 6 - 3 when player A wons 6 \"jeux\" in player B 3 \"jeux\"" $
      scoreOfGame [
      A, A, A, A,
      A, A, A, A,
      B, B, B, B,
      A, A, A, A,
      A, A, A, A,
      B, B, B, B,
      A, A, A, A,
      B, B, B, B,
      A, A, A, A] `shouldBe` Match (Zero, Zero) (Set 6 3) (Set 0 0)

  describe "How a complete game is played in a tennis game "$ do
    it "returns 6 - 0 1 - 0 when player A wons 6 \"jeux\" in first set and 1 \"jeux\" in the second set" $
      scoreOfGame [
      A, A, A, A,
      A, A, A, A,
      A, A, A, A,
      A, A, A, A,
      A, A, A, A,
      A, A, A, A,
      A, A, A, A] `shouldBe` Match (Zero, Zero) (Set 6 3) (Set 1 0)

  -- Checking the set is won with 2 points ahead until 6

data ScoreJeu = Zero | Quinze | Trente | Quarante | Avantage deriving (Show, Eq, Enum)
data Set = Set Integer Integer deriving (Show, Eq)
data Match = Match (ScoreJeu, ScoreJeu) Set Set deriving (Show, Eq)
data Player = A | B deriving (Show, Eq)

-- Pattern matching
{-score :: [Player] -> (Int, Int)-}
{-score [] = (0, 0)-}
{-score [A] = (15, 0)-}
{-score [A, A] = (30, 0)-}
{-score [A, A, A] = (40, 0)-}
{-score [A, A, B, A] = (40, 15)-}
{-score [A, B, A, B, A] = (40, 15)-}

-- Recursion + Pattern matching
-- score :: [Player] -> Match -> Match
-- score [] (Match (pointsA, pointsB) (Set jeuxA1 jeuxB1)) = Match (pointsA, pointsB) (Set jeuxA1 jeuxB1)
-- score (A:tail) (Match (Quarante, _) (Set jeuxA1 jeuxB1)) = score tail (Match (Zero, Zero) (Set (jeuxA1 + 1) jeuxB1))
-- score (B:tail) (Match (_, Quarante) (Set jeuxA1 jeuxB1)) = score tail (Match (Zero, Zero) (Set jeuxA1 (jeuxB1 + 1)))
-- score (A:tail) (Match (pointsA, pointsB) (Set jeuxA1 jeuxB1)) = score tail (Match (succ pointsA, pointsB) (Set jeuxA1 jeuxB1))
-- score (B:tail) (Match (pointsA, pointsB) (Set jeuxA1 jeuxB1)) = score tail (Match (pointsA, succ pointsB) (Set jeuxA1 jeuxB1))

scoreOfGame :: [Player] -> Match
scoreOfGame hits = score hits (Match (Zero, Zero) (Set 0 0) (Set 0 0))

-- Recursion + guards + deuce
score :: [Player] -> Match -> Match
score [] (Match (pointsA, pointsB) (Set jeuxA1 jeuxB1) (Set jeuxA2 jeuxB2)) = Match (pointsA, pointsB) (Set jeuxA1 jeuxB1) (Set jeuxA2 jeuxB2)
score (A:tail) (Match (pointsA, pointsB) (Set jeuxA1 jeuxB1) (Set jeuxA2 jeuxB2))
  | pointsA == Quarante && pointsB == Avantage = score tail (Match (Quarante, Quarante) (Set jeuxA1 jeuxB1) (Set jeuxA2 jeuxB2))
  | pointsA == Quarante && pointsB == Quarante = score tail (Match (Avantage, Quarante) (Set jeuxA1 jeuxB1) (Set jeuxA2 jeuxB2))
  | pointsA == Avantage || pointsA == Quarante = score tail (Match (Zero, Zero) (Set (jeuxA1 + 1) jeuxB1) (Set jeuxA2 jeuxB2))
  | otherwise = score tail (Match (succ pointsA, pointsB) (Set jeuxA1 jeuxB1) (Set jeuxA2 jeuxB2))

score (B:tail) (Match (pointsA, pointsB) (Set jeuxA1 jeuxB1) (Set jeuxA2 jeuxB2))
  | pointsB == Quarante && pointsA == Avantage = score tail (Match (Quarante, Quarante) (Set jeuxA1 jeuxB1) (Set jeuxA2 jeuxB2))
  | pointsB == Quarante && pointsA == Quarante = score tail (Match (Quarante, Avantage) (Set jeuxA1 jeuxB1) (Set jeuxA2 jeuxB2))
  | pointsB == Avantage || pointsB == Quarante = score tail (Match (Zero, Zero) (Set jeuxA1 (jeuxB1 + 1)) (Set jeuxA2 jeuxB2))
  | otherwise = score tail (Match (pointsA, succ pointsB) (Set jeuxA1 jeuxB1) (Set jeuxA2 jeuxB2))
