{-# LANGUAGE ScopedTypeVariables #-}
module Math.Programming.Tests.LP where

import Control.Monad
import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

import Math.Programming

makeLPTests
  :: (PrintfArg a, RealFrac a, MonadIO m, LPMonad m a)
  => (m () -> IO ())  -- ^ The runner for the API being tested.
  -> TestTree         -- ^ The resulting test suite.
makeLPTests runner = testGroup "LP problems"
  [ testCase "Diet problem" (runner dietProblemTest)
  ]

data Food = Corn | Milk | Bread
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

data Nutrient = Calories | VitaminA
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

dietProblemTest :: forall a m. (PrintfArg a, RealFrac a, MonadIO m, LPMonad m a) => m ()
dietProblemTest =
  let
    cost :: Food -> a
    cost Corn = 0.18
    cost Milk = 0.23
    cost Bread = 0.05

    nutrition :: Nutrient -> Food -> a
    nutrition Calories Corn = 72
    nutrition VitaminA Corn = 107
    nutrition Calories Milk = 121
    nutrition VitaminA Milk = 500
    nutrition Calories Bread = 65
    nutrition VitaminA Bread = 0

    foods :: [Food]
    foods = [Corn, Milk, Bread]

    nutrients :: [Nutrient]
    nutrients = [Calories, VitaminA]

    maxServings :: a
    maxServings = 10

    nutrientBounds :: Nutrient -> (a, a)
    nutrientBounds Calories = (2000, 2250)
    nutrientBounds VitaminA = (5000, 50000)

    expected :: Food -> a
    expected Corn = 1.94
    expected Milk = 10
    expected Bread = 10

    expectedCost :: a
    expectedCost = 3.15

    amountInterval :: Bounds a
    amountInterval = Interval 0 maxServings

    amountName :: Food -> String
    amountName food = printf "amount[%s]" (show food)

    nutrientMaxName :: Nutrient -> String
    nutrientMaxName nutrient = printf "%s_max" (show nutrient)

    nutrientMinName :: Nutrient -> String
    nutrientMinName nutrient = printf "%s_min" (show nutrient)

  in do
    -- Create the decision variables
    amounts <- forM foods $ \food -> do
      v <- addVariable `within` amountInterval `named` (amountName food)
      return (food, v)

    -- Create the nutrient constraints
    forM_ nutrients $ \nutrient -> do
      let lhs = sumExpr [nutrition nutrient food *: v | (food, v) <- amounts]
          (lower, upper) = nutrientBounds nutrient
      _ <- (addConstraint $ lhs .<= upper) `named` (nutrientMaxName nutrient)
      (addConstraint $ lhs .>= lower) `named` (nutrientMinName nutrient)

    -- Set the objective
    let objective = sumExpr [cost food *: v | (food, v) <- amounts]
    setObjective objective
    setSense Minimization

    -- Solve the problem
    status <- optimizeLP

    -- Check that we reached optimality
    liftIO $ status @?= Optimal

    -- Check the variable values
    forM_ amounts $ \(food, v) -> do
      x <- getValue v

      let correct = expected food
          msg = printf
                "Amount of %s should be about %.2f, but is %.3f"
                (show food)
                (realToFrac correct :: a)
                (realToFrac x :: a)
      liftIO $ assertBool msg (abs (x - correct) <= 1e-1)

    -- Check the objective value
    objectiveValue <- evalExpr objective
    let msg = printf
              "Objective should be about %.2f, but is %.3f"
              expectedCost
              objectiveValue
    liftIO $ assertBool msg (abs (objectiveValue - expectedCost) < 1e-1)