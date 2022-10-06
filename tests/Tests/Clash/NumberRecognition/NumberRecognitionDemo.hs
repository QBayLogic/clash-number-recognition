{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Tests.Clash.NumberRecognition.NumberRecognitionDemo where


import Clash.Prelude
import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as T

import qualified Data.List as L
import qualified Data.Ord as Ord (comparing)
import Data.Maybe (catMaybes)
import Debug.Trace
import System.IO

import NumberRecognition.CameraInterface (d8mProcessing)
import NumberRecognition.NeuralNetwork (neuralNetwork)
import NumberRecognition.NumberRecognitionDemo (topEntity)
import Tests.Clash.NumberRecognition.CameraInterface 
  (loadPixeldata, loadVS, loadHS)


tests :: [T.TestTree]
tests =
  [
  ]


testbench = onlyNew . fmap (\ (a,_,_) -> a) <$> nnOut
  where
    nnOut = do
      px <- loadPixeldata
      vs <- loadVS
      simulateN (L.length px) dut . L.zip3 px vs <$> loadHS
    dut (unbundle -> (px, vs, hs)) = topEntity hasClock (pure True) px vs hs

onlyNew :: Eq a => [a] -> [a]

onlyNew [x] = [x]
onlyNew (x:xs)
  | x == L.head xs = onlyNew xs
  | otherwise = x : onlyNew xs
onlyNew [] = []