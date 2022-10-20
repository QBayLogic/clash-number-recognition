{-| 
  Copyright: (C) 2022, QBayLogic B.V.
  License:   see LICENSE

  The NeuralNetwork module provides functionality for evaluating a feedforward
  neural network. The neural network is to be trained externally and the weights
  and biases are to be exported as text files. During development, a model 
  was trained in Python using TensorFlow. This model was able to recognise 
  handwritten numbers and was trained using the MNIST database. 
-}

{-# LANGUAGE FlexibleContexts #-}

module NumberRecognition.NeuralNetwork 
  (-- * Types
    PxVal
  , NNParam
  , OutputVec
  , HexDigit
    -- ** Address 'Index' types
  , InputAddress
  , HiddenLayerAddress
  , OutputAddress
  , WeightAddress
  , BiasAddress
    -- * Data types
  , NetworkState
    -- * Functions
  , neuralNetwork
  , toSevenSegment
  , elemMax
  , toNNParam
  , relu
  , mac
  )
where

import Clash.Prelude
import Data.Maybe (fromJust, isJust)

import NumberRecognition.NNConfig 
  ( InputNodes
  , HiddenNodes
  , OutputNodes
  , WeightType
  , BiasType
  )

type WeightsLength = InputNodes * HiddenNodes + HiddenNodes * OutputNodes
type BiasesLength = HiddenNodes + OutputNodes

type InputAddress = Index InputNodes
type HiddenLayerAddress = Index HiddenNodes
type OutputAddress = Index OutputNodes
type WeightAddress = Index (InputNodes * HiddenNodes + HiddenNodes * OutputNodes)
type BiasAddress = Index (HiddenNodes + OutputNodes)
type InFirstLayer = Bool

type PxVal = Unsigned 8
type NNParam = SFixed 8 8
type OutputVec = Vec OutputNodes NNParam
type HexDigit = BitVector 7

data NetworkState = FirstLayer  (InputAddress, HiddenLayerAddress)
                  | SecondLayer (HiddenLayerAddress, OutputAddress)
                  | Waiting
                  deriving (Generic, NFDataX, Show)

weightPath :: [Char]
weightPath = "src/NumberRecognition/weights.dat"
biasPath :: [Char]
biasPath = "src/NumberRecognition/biases.dat"


-- | Evaluate a neural network
--
-- Receives pixel values and their addresses. Once the last pixel is received,
-- it starts evaluating the data to recognise the handwritten number in the 
-- frame.
-- 
-- To perform the evaluation, this function needs two text files: 
-- @weights.dat@ and @biases.dat@, both placed in the same directory as this 
-- file. 
-- These files contain the parameters of the pre-trained neural network. Both 
-- files should contain newline separated values in the 'NNParam' format, 
-- binary encoded. (e.g. for @SFixed 8 8@, the files should contain strings of
-- 16 ones and zeros). The files should contain (at least) 'WeightsLength' and 
-- 'BiasesLength' parameters respectively.
neuralNetwork
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom (Maybe (InputAddress, PxVal))
  -- ^ Greyscale pixel value and respective address, wrapped in Maybe
  -> Signal dom OutputVec
  -- ^ Vector of 10 values which can be interpreted as a \'chance\'
neuralNetwork (fmap (fmap (fmap toNNParam)) -> inputWriter) = outputVec
  where
    lastPixel = (fmap fst <$> inputWriter) .==. pure (Just maxBound)
    (state, nodeBegin) = unbundle $ register (Waiting, False) (stateMachine <$> state <*> lastPixel)
    (inpLayer, inpAddr, hiddenAddr, outAddr, weightAddr, biasAddr) = unbundle (stateToAddr <$> state)

    nodeBegin' = register False nodeBegin
    inpLayer' = register True inpLayer
    inpLayer'' = register True inpLayer'
    hiddenAddr' = register 0 hiddenAddr
    hiddenAddr'' = register 0 hiddenAddr'
    outAddr' = register 0 outAddr
    outAddr'' = register 0 outAddr'

    weight :: Signal dom NNParam
    weight = resizeF <$> weightRom
      where
        weightRom :: Signal dom WeightType
        weightRom = unpack <$> romFile (SNat @WeightsLength) weightPath weightAddr
    bias :: Signal dom NNParam
    bias      = resizeF <$> biasRom
      where
        biasRom :: Signal dom BiasType
        biasRom = unpack <$> romFile (SNat @BiasesLength) biasPath biasAddr
    maybeBias = mux nodeBegin' (fmap Just bias) (pure Nothing)

    inputVal  = blockRamU NoClearOnReset (SNat @InputNodes) (const (deepErrorX "")) inpAddr inputWriter
    hiddenVal = blockRam (replicate (SNat @HiddenNodes) 0) hiddenAddr hiddenWriter
    maInput   = mux inpLayer' inputVal hiddenVal
    ma        = mealy mac 0 (bundle (maInput, weight, maybeBias))

    hiddenWriter = mux (nodeBegin' .&&. inpLayer'') (Just <$> bundle (hiddenAddr'', relu <$> ma)) (pure Nothing)
    outputVec = mealy nodeWriter (repeat 0, repeat 0) (bundle (nodeBegin', inpLayer'', outAddr'', ma))
{-# NOINLINE neuralNetwork #-}

-- | Keep a locked output until a complete /Vector/ is updated
nodeWriter
  :: (OutputVec, OutputVec)
  -> (NewNodeFlag, InFirstLayer, OutputAddress, NNParam)
  -> ((OutputVec, OutputVec), OutputVec)
nodeWriter (current, lockedOutput) (nodeBegin, inFirstLayer, outAddr, nodeValue)
  | nodeBegin && not inFirstLayer && outAddr == maxBound = ((outputVec, outputVec), lockedOutput)
  | nodeBegin && not inFirstLayer = ((outputVec, lockedOutput), lockedOutput)
  | otherwise = ((current, lockedOutput), lockedOutput)
  where
    outputVec = replace outAddr nodeValue current

-- TODO: Rewrite to record syntax
stateToAddr
  :: NetworkState
  -> (InFirstLayer,
      InputAddress,
      HiddenLayerAddress,
      OutputAddress,
      WeightAddress,
      BiasAddress
  )
stateToAddr state = case state of
  FirstLayer (inp, node)
    -> (True, inp, node, undefinedAddrOut, wAddr, resize node)
    where
      wAddr = resize inp + (natToNum @InputNodes * resize node)
  SecondLayer (node, out)
    -> (False, undefinedAddrHidden, node, out, wAddr, bAddr)
    where
      wAddr = resize node + (natToNum @(InputNodes * HiddenNodes) + natToNum @HiddenNodes * resize out)
      bAddr = natToNum @HiddenNodes + resize out
  Waiting
    -> (False, 0, 0, 0, 0, 0)
 where
  undefinedAddrOut = deepErrorX "stateToAddr: output address is undefined."
  undefinedAddrHidden = deepErrorX "stateToAddr: hidden-layer address is undefined."


type NewNodeFlag = Bool
type FullFrameFlag = Bool

stateMachine
  :: NetworkState         
  -- ^ Current state
  -> FullFrameFlag
  -- ^ Flag to indicate the last pixel has been received
  -> (NetworkState, NewNodeFlag) 
  -- ^ Bundled updated state and a flag to indicate a new node is starting
stateMachine state lastPixel = case state of
  FirstLayer (inp, node)
    | node == maxBound && inp == maxBound -- End of first layer
    -> (SecondLayer (0, 0), True)
    | inp == maxBound   -- End of node in first layer
    -> (FirstLayer (0, succ node), True)
    | otherwise
    -> (FirstLayer (succ inp, node), False)
  SecondLayer (inp, node)
    | node == maxBound && inp == maxBound  -- End of second layer
   -> (Waiting, True)
    | inp == maxBound   -- End of node in second layer
    -> (SecondLayer (0, succ node), True)
    | otherwise
    -> (SecondLayer (succ inp, node), False)
  Waiting
    | lastPixel
    -> (FirstLayer (0,0) , True)
    | otherwise
    -> (Waiting, False)

-- | Multiply Accumulate
--
-- Muliplies the two given inputs and adds them to the accumulator. Resets the 
-- accumulator to the value inside the Just if it has one. 
mac 
  :: NNParam 
  -> (NNParam, NNParam, Maybe NNParam) 
  -> (NNParam, NNParam)
mac acc (x, y, newAcc)
  | isJust newAcc = (ma (fromJust newAcc) x y, acc)
  | otherwise     = (ma acc x y, acc)
  where
    ma a b c = a + b * c

-- | Rectified Linear Unit (ReLU)
--
-- ReLU is a non-linear activation function. It returns the maximum value between
-- zero and the input value.
relu :: (Num a, Ord a) => a -> a
relu x
  | x > 0     = x
  | otherwise = 0

-- | Replaces softmax activation to get the index of the maximum element
elemMax :: (KnownNat n, Num a, Ord a, 1 <= n) => Vec n a -> Index n
elemMax = fst . ifoldr maxValIdx (0, 0)
  where
    maxValIdx curIdx curVal (maxIdx,maxVal)
      | curVal > maxVal = (curIdx, curVal)
      | otherwise       = (maxIdx, maxVal)

-- | Convert a decimal number to a 7-bit 'BitVector' for a 7-segment display
toSevenSegment :: Index 10 -> HexDigit
toSevenSegment inp = case inp of
  0  -> 0b0111111
  1  -> 0b0000110
  2  -> 0b1011011
  3  -> 0b1001111
  4  -> 0b1100110
  5  -> 0b1101101
  6  -> 0b1111101
  7  -> 0b0000111
  8  -> 0b1111111
  9  -> 0b1101111
  _  -> 0b0001000

-- | Map the input to [0-1) and change its type.
--
-- Converts a given `Unsigned a` value to an `SFixed b a`. 
toNNParam 
  :: forall int1 frac1 . (KnownNat int1, KnownNat frac1)
  => Unsigned frac1 -> SFixed int1 frac1
toNNParam a = resizeF (unpack (zeroExtend (pack a)) :: SFixed 1 frac1)