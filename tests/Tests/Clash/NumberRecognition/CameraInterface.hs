{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Tests.Clash.NumberRecognition.CameraInterface where

import Prelude
import Clash.Prelude (Signal, HiddenClockResetEnable, System)
import qualified Clash.Prelude as P
import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as T

import qualified Data.List as L
import qualified Data.Ord as Ord (comparing)

import App.CameraInterface


tests :: [T.TestTree]
tests =
  [
  ]


-- Helper functions for testing
yxs :: HiddenClockResetEnable dom => Signal dom (Ycounter, Xcounter)
yxs = P.register (185,0) (yxCounter <$> yxs)

outState :: HiddenClockResetEnable dom => Signal dom BayerState
outState = P.register (IgnorePixel (0,0)) (bayerStateMachine <$> outState <*> yxs)

simulateBayer :: Int -> [((Ycounter, Xcounter), BayerState)]
simulateBayer n = L.zip (P.sampleN @System n yxs) (P.sampleN @System n outState)

showBayerSimulation :: Int -> Int -> IO()
showBayerSimulation n showNr = putStr (unlines (L.map show out))
  where
    out = lastN showNr (simulateBayer n)

coordCntr ::
  HiddenClockResetEnable dom =>
  Signal dom (VS, HS) ->
  Signal dom (Ycounter, Xcounter)
coordCntr inp = P.mealy coordinateCounter (False, False, 0, 0) inp


yxCounter :: (Ycounter, Xcounter) -> (Ycounter, Xcounter)
yxCounter (y,x)
  | x == maxBound = (P.satSucc P.SatWrap y, 0)
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

lastXY :: Int -> (Ycounter, Xcounter)
lastXY n = L.last $ L.take n $ P.simulate @P.System coordCntr syncPattern

maxY :: [(Ycounter, Xcounter)] -> (Ycounter, Xcounter)
maxY a = L.maximumBy (Ord.comparing fst) a

maxX :: [(Ycounter, Xcounter)] -> (Ycounter, Xcounter)
maxX a = L.maximumBy (Ord.comparing snd) a

lastN :: Int -> [a] -> [a]
lastN n xs = L.drop (L.length xs - n) xs
