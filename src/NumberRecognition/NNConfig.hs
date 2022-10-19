module NumberRecognition.NNConfig where

import Clash.Prelude

-- Nodes per layer in the neural network
type HPixelCount = 28
type InputNodes  = 784
type HiddenNodes = 100
type OutputNodes = 10

-- Types of the weights and biases parameters (binary encoded in .dat)
type WeightType = SFixed 3 8
type BiasType   = SFixed 1 8
