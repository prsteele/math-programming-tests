module Math.Programming.Tests.Api where

import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

import Math.Programming

makeApiTests
  :: (PrintfArg a, RealFrac a, MonadIO m, LPMonad m a)
  => (m () -> IO ())  -- ^ The runner for the API being tested.
  -> TestTree         -- ^ The resulting test suite.
makeApiTests runner = testGroup "API tests"
  [ testCase "Set/get variable names" (runner setGetVariableName)
  , testCase "Set/get constraint names" (runner setGetConstraintName)
  ]

setGetVariableName :: (MonadIO m, LPMonad m a) => m ()
setGetVariableName = do
  let name = "foo"
  x <- addVariable `named` name
  vName <- getVariableName x
  liftIO $ vName @?= name

setGetConstraintName :: (MonadIO m, LPMonad m a) => m ()
setGetConstraintName = do
  let name = "foo"
  x <- addVariable
  c <- addConstraint (1 *: x .>= 0) `named` name
  cName <- getConstraintName c
  liftIO $ cName @?= name
