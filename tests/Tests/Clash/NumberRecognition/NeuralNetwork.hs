{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Tests.Clash.NumberRecognition.NeuralNetwork where

import Clash.Prelude
import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as T

import qualified Data.List as L

import NumberRecognition.NNConfig 
  ( InputNodes
  , HiddenNodes
  , OutputNodes
  )
import NumberRecognition.NeuralNetwork
  ( NNParam
  , neuralNetwork
  , elemMax
  )

tests :: [T.TestTree]
tests =
  [
  ]

type NNDelay = 4
type SimDuration = InputNodes * HiddenNodes + HiddenNodes * OutputNodes + NNDelay

-- | Simulates one iteration of the neural network. 
-- 
-- The input image is an image from the MNIST database and is preloaded in its 
-- BlockRam from the file "NumberRecognition.InputImageVector". 
-- The simulation needs @InputNodes * HiddenNodes@ cycles to calculate the nodes 
-- in the hidden layer, and @HiddenNodes * OutputNodes@ for the output layer, 
-- and has a delay of 'NNDelay'. The total simulation duration is given by 
-- 'SimDuration'.
-- Each index of the output vector represents the 'chance' that that number
-- is recognized in the image. These values are *not* normalized with softmax.
neuralNetSim :: Vec 10 NNParam
neuralNetSim = L.last $ simulateN @System (natToNum @SimDuration) neuralNetwork (pure Nothing)

nnNumber :: Index 10
nnNumber = elemMax neuralNetSim
