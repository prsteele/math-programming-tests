{-# LANGUAGE FlexibleContexts #-}
module Math.Programming.Tests where

import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

import Math.Programming

makeIPTests
  :: (PrintfArg a, RealFrac a, MonadIO m, IPMonad m a)
  => String           -- ^ The name of the API being tested. This will
                      -- be used to generate test group names.
  -> (m () -> IO ())  -- ^ The runner for the API being tested.
  -> TestTree         -- ^ The resulting test suite.
makeIPTests apiName runner
  = testGroup (printf "Simple MIP problems (%s)" apiName)
    [ testCase "Simple MIP" (runner simpleMIPTest)
    ]

-- | We solve a simple MIP of the form
--
-- @
-- min  x + y
-- s.t. x >= 1.1
--      y >= 1.1
--      0 <= x <= 5
--      0 <= y <= 5
--      x integer
-- @
--
-- The optimal solution to this MIP is x = 2, y = 1.1.
simpleMIPTest :: (PrintfArg a, RealFrac a, MonadIO m, IPMonad m a) => m ()
simpleMIPTest = do
  x <- addVariable `asKind` Integer `within` Interval 0 5
  y <- addVariable `asKind` Continuous `within` Interval 0 5
  _ <- addConstraint (1 *: x .>= 1.1)
  _ <- addConstraint (1 *: y .>= 1.1)
  setObjective (1 *: x .+. 1 *: y)
  setSense Minimization
  _ <- optimizeLP
  status <- optimizeIP

  -- Check that we reached optimality
  liftIO $ status @?= Optimal

  vx <- getValue x
  let xmsg = printf "Expected x to be 2, but is %.3f" vx
  liftIO $ assertBool xmsg (abs (vx - 2) <= 1e-1)

  vy <- getValue y
  let ymsg = printf "Expected y to be 1.1, but is %.3f" vy
  liftIO $ assertBool ymsg (abs (vy - 1.1) <= 1e-1)
