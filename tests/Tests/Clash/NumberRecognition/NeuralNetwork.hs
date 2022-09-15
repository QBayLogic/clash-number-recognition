{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Tests.Clash.NumberRecognition.NeuralNetwork where

import Clash.Prelude
import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as T

import qualified Data.List as L

import App.NNParamsList (NNParam)
import App.NeuralNetwork

tests :: [T.TestTree]
tests =
  [
  ]

-- Simulates one iteration of the neural network. The input image is an image 
-- from the MNIST database and is preloaded in its Block Ram from the file 
-- `App.InputImageVector.hs`. The simulation needs 7840 cycles to calculate the
-- nodes in the hidden layer, and 100 for the output layer, and has a 3 cycle 
-- delay, thus resulting in the value 7943. 
-- Each index of the output vector represents the 'chance' that that number
-- is recognized in the image. These values are *not* normalized with softmax.
neuralNetSim :: Vec 10 NNParam
neuralNetSim = L.last $ sampleN @System 7943 neuralNet

nnNumber :: Index 10
nnNumber = elemMax neuralNetSim