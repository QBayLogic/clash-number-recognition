{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module App.NeuralNetwork where

import Clash.Prelude
import Clash.Intel.ClockGen
import Clash.Annotations.SynthesisAttributes

import Data.Maybe

import App.CameraInterface (PxVal)
import App.NNParamsList (NNParam, biasesList, weightsList)
import App.InputImageVector (inputImage)


-- FirstLayer (selectedInput, selectedNode)
-- SecondLayer (selectedInput, selectedNode)
data NetworkState = FirstLayer (Index 784, Index 10)
                  | SecondLayer (Index 10, Index 10)
                  | Waiting
                  deriving (Generic, NFDataX, Show)

type HexDigit = BitVector 7


-- A 50 MHz clock (period=20,000 ps)
createDomain vSystem{vName="Dom50MHz", vPeriod=20000}
-- A 20 MHz clock (period=50,000 ps)
createDomain vSystem{vName="Dom20MHz", vPeriod=50000}


{-# ANN topEntity
  (Synthesize
    { t_name   = "neuralNetwork"
    , t_inputs = [ PortName "CLOCK_50"
                 , PortName "KEY0"
                 ]
    , t_output = PortProduct "" [PortName "LEDR", PortName "HEX0"]
    })#-}

topEntity
  :: Clock Dom50MHz
      `Annotate` 'StringAttr "chip_pin" "AF14"
  -> Signal Dom50MHz Bool
      `Annotate` 'StringAttr "chip_pin" "AJ4"
  -> Signal Dom50MHz (BitVector 10, HexDigit)
topEntity clk rst = exposeClockResetEnable go clk (unsafeFromLowPolarity rst) enableGen
 where
  go :: HiddenClockResetEnable Dom50MHz
    => Signal Dom50MHz (BitVector 10, HexDigit)
  go = bundle (led_out, hex_out)
    where
      led_out = pack <$> pwm10 nn_output
      hex_out = complement . toSevenSegment . elemMax <$> nn_output
      nn_output = neuralNet


neuralNet :: HiddenClockResetEnable dom => Signal dom (Vec 10 NNParam)
neuralNet = outputVec
  where
    (state, nodeBegin) = unbundle $ register (FirstLayer (0, 0), True) (stateMachine <$> state)
    (inpAddr, weightAddr, biasAddr)  = unbundle $ stateToAddress <$> state
    nodeBegin' = register False nodeBegin
    biasAddr'' = register 0 $ register 0 biasAddr
    weightBlob = $(memBlobTH Nothing weightsList)
    biasBlob  = $(memBlobTH Nothing biasesList)
    input     = blockRam inputImage inpAddr hiddenNode
    weight    = unpack <$> blockRamBlob weightBlob weightAddr (pure Nothing)
    bias      = unpack <$> blockRamBlob biasBlob biasAddr (pure Nothing)
    maybeBias = mux nodeBegin' (fmap Just bias) (pure Nothing)
    ma        = mealy mac 0 (bundle (input, weight, maybeBias))
    (hiddenNode, outputVec)  = unbundle (mealy nodeWriter (replicate d10 0) (bundle (nodeBegin', biasAddr'', ma)))


data Direction = Up | Down
  deriving (Generic, NFDataX)

pwm10 ::
  forall dom u p . (HiddenClockResetEnable dom, KnownNat u, KnownNat p)
  => Signal dom (Vec 10 (SFixed u p))
  -> Signal dom (Vec 10 Bool)
pwm10 inp = mealy go (0, Up) inp
  where
    go state i = (newState, out)
     where
      (counter, direction) = state
      newState = (newCounter, newDirection)
      newDirection
        | allOn = Up
        | allOff = Down
        | otherwise = direction
      out = fmap (>counter) i
      allOn = and out
      allOff = not $ or out
      newCounter = satAdd SatWrap counter $ case newDirection of
        Up -> unpack 0b1
        Down -> unpack (maxBound :: BitVector (u+p))


nodeWriter
  -- Curent state: input vector
  :: Vec 10 NNParam
  -- Input: (nodeBegin, biasAddr, nodeValue)
  -> (Bool, Index 20, NNParam)
  -- (updated state, output)
  -> (Vec 10 NNParam, (Maybe (Index 794, NNParam), Vec 10 NNParam))
nodeWriter inputVec (nodeBegin, biasAddr, nodeValue)
  | nodeBegin && (biasAddr < 10)
    = (inputVec, (Just (784 + resize biasAddr, relu nodeValue), inputVec))
  | nodeBegin && (biasAddr > 9)
    = (outputVec, (Nothing, outputVec))
  | otherwise
    = (inputVec, (Nothing, inputVec))
  where
    outputVec = replace (resize (biasAddr - 10)::Index 10) nodeValue inputVec


stateToAddress
  :: NetworkState
  -> (Index 794,  -- inputAddress
      Index 7940, -- weightAddress
      Index 20)   -- biasAddress
stateToAddress state = case state of
  FirstLayer (inp, node)
    -> (resize inp, resize inp + (784 * resize node), resize node)
  SecondLayer (inp, node)
    -> (resize inp + 784, resize inp + (7840 + 10 * resize node), resize node + 10)
  Waiting
    -> (0, 0, 0)


stateMachine
  :: NetworkState         -- Current state
  -> (NetworkState, Bool) -- (New state, new node flag)
stateMachine state = case state of
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
    -> (Waiting, False)


mac :: NNParam -> (NNParam, NNParam, Maybe NNParam) -> (NNParam, NNParam)
mac acc (x, y, newAcc)
  | isJust newAcc = (ma (fromJust newAcc) x y, acc)
  | otherwise     = (ma acc x y, acc)
  where
    ma a b c = a + b * c


toNNParam :: PxVal -> NNParam
toNNParam a = resizeF (unpack (zeroExtend (pack a)) :: SFixed 1 8)

relu :: (Num a, Ord a) => a -> a
relu x
  | x > 0     = x
  | otherwise = 0

-- Replaces softmax activation to just get the index of the maximum element
elemMax :: (KnownNat n, Num a, Ord a, 1 <= n) => Vec n a -> Index n
elemMax = fst . ifoldr maxValIdx (0, 0)
  where
    maxValIdx curIdx curVal (maxIdx,maxVal)
      | curVal > maxVal = (curIdx, curVal)
      | otherwise       = (maxIdx, maxVal)

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
