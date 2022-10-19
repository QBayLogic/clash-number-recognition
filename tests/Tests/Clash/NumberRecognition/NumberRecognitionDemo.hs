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
import NumberRecognition.NeuralNetwork (neuralNetwork, HexDigit)
import NumberRecognition.NumberRecognitionDemo (topEntity)
import Tests.Clash.NumberRecognition.CameraInterface 
  (loadPixeldata, loadVS, loadHS)


tests :: [T.TestTree]
tests =
  [
  ]

-- | Simulate the camera interface and neural network. 
--
-- Uses 3 text files containing a pixel stream to simulate the input from the
-- D8M camera module. This stream is processed and given to the neural network.
-- The output of this function is the complement of a 'BitVector' which would
-- be shown on a 7-segment display. The output of this function should show
-- the number(s) detected in the pixel stream.
testbench :: IO [Index 10]
testbench = fmap (fmap (fromSevenSegment . complement)) $ onlyNew . fmap (\ ((a,_),_,_) -> a) <$> nnOut
  where
    nnOut = do
      px <- loadPixeldata
      vs <- loadVS
      simulateN (L.length px) dut . L.zip3 px vs <$> loadHS
    dut (unbundle -> (px, vs, hs)) = topEntity hasClock (pure True) px vs hs

-- | Remove duplicates from a list
onlyNew :: Eq a => [a] -> [a]
onlyNew [x] = [x]
onlyNew (x:xs)
  | x == L.head xs = onlyNew xs
  | otherwise = x : onlyNew xs
onlyNew [] = []

-- | Convert a 7-bit 'BitVector' for a 7-segment display to a decimal number 
fromSevenSegment :: HexDigit -> Index 10
fromSevenSegment inp = case inp of
  0b0111111 -> 0
  0b0000110 -> 1
  0b1011011 -> 2
  0b1001111 -> 3
  0b1100110 -> 4
  0b1101101 -> 5
  0b1111101 -> 6
  0b0000111 -> 7
  0b1111111 -> 8
  0b1101111 -> 9
  _ -> deepErrorX "fromSevenSegment: Invalid 7-segment output"