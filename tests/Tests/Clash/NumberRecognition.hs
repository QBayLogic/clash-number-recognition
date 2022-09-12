module Tests.Clash.NumberRecognition where

import Prelude
import Test.Tasty

import qualified Tests.Clash.NumberRecognition.CameraInterface
import qualified Tests.Clash.NumberRecognition.NeuralNetwork

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Clash.NumberRecognition"
  [ testGroup "CameraInterface" Tests.Clash.NumberRecognition.CameraInterface.tests
  , testGroup "NeuralNetwork" Tests.Clash.NumberRecognition.NeuralNetwork.tests
  ]
