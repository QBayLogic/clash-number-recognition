{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Tests.Clash.NumberRecognition.CameraInterface where


import Clash.Prelude
import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as T

import qualified Data.List as L
import qualified Data.Ord as Ord (comparing)
import Data.Maybe (catMaybes)
import Debug.Trace
import System.IO

import App.CameraInterface
  (VS, HS, YCounter, XCounter, PxDataRaw, BayerState (IgnorePixel),
  d8mProcessing, coordinateCounter, bayerStateMachine)
import App.NeuralNetwork (PxVal, InputAddress)


tests :: [T.TestTree]
tests =
  [
  ]


d8mSimVals :: IO [PxVal]
d8mSimVals = fmap (fmap snd) d8mSim

d8mSim :: IO [(InputAddress, PxVal)]
d8mSim = out
  where
    out = catMaybes <$> outVal
    outVal = do
      px <- loadPixeldata
      vs <- loadVS
      hs <- loadHS
      return (simulateN @System (L.length px) d8mHelper (L.zip3 px vs hs))

d8mHelper (unbundle -> (px, vs, hs)) = d8mProcessing px' yx
  where
    px' = register 0 px
    yx = mealy coordinateCounter (False, False, 0, 0) (bundle (vs, hs))

loadPixeldata :: IO [PxDataRaw]
loadPixeldata = do
  let filepath = "tests/Tests/Clash/NumberRecognition/pixeldata.txt"
  fmap (pack . flip shiftL 2 . read @(Unsigned 10)) . lines <$> readFile filepath

loadVS :: IO [VS]
loadVS = do
  let filepath = "tests/Tests/Clash/NumberRecognition/vs.txt"
  fmap ((> 0) . read @(Unsigned 1)) . lines <$> readFile filepath

loadHS :: IO [HS]
loadHS = do
  let filepath = "tests/Tests/Clash/NumberRecognition/hs.txt"
  fmap ((> 0) . read @(Unsigned 1)) . lines <$> readFile filepath



-- Helper functions for testing
yxs :: HiddenClockResetEnable dom => Signal dom (YCounter, XCounter)
yxs = register (185,0) (yXCounter <$> yxs)

outState :: HiddenClockResetEnable dom => Signal dom BayerState
outState = register (IgnorePixel (0,0)) (bayerStateMachine <$> outState <*> yxs)

simulateBayer :: Int -> [((YCounter, XCounter), BayerState)]
simulateBayer n = L.zip (sampleN @System n yxs) (sampleN @System n outState)

showBayerSimulation :: Int -> Int -> IO ()
showBayerSimulation n showNr = putStr (unlines (L.map show out))
  where
    out = lastN showNr (simulateBayer n)

coordCntr ::
  HiddenClockResetEnable dom =>
  Signal dom (VS, HS) ->
  Signal dom (YCounter, XCounter)
coordCntr inp = mealy coordinateCounter (False, False, 0, 0) inp


yXCounter :: (YCounter, XCounter) -> (YCounter, XCounter)
yXCounter (y,x)
  | x == maxBound = (satSucc SatWrap y, 0)
  | otherwise     = (y, succ x)

syncPattern :: [(VS, HS)]
syncPattern =
  L.replicate 400 (False, False)
    <> L.replicate (45 * 793) (True, False)
    <> L.concat
      ( L.replicate
          480
          ( L.replicate 152 (True, False)
              <> L.replicate 640 (True, True)
          )
      )
    <> [(True, False)]
    <> L.repeat (False, False)

lastXY :: Int -> (YCounter, XCounter)
lastXY n = L.last $ L.take n $ simulate @System coordCntr syncPattern

maxY :: [(YCounter, XCounter)] -> (YCounter, XCounter)
maxY a = L.maximumBy (Ord.comparing fst) a

maxX :: [(YCounter, XCounter)] -> (YCounter, XCounter)
maxX a = L.maximumBy (Ord.comparing snd) a

lastN :: Int -> [a] -> [a]
lastN n xs = L.drop (L.length xs - n) xs
