{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module App.NeuralNetwork where

import Clash.Prelude
import Clash.Intel.ClockGen
import Clash.Annotations.SynthesisAttributes

import Data.Maybe
import Debug.Trace
import qualified Data.List as L

import App.NNParamsList (NNParam, biasesList, weightsList)
import App.InputImageVector (inputImage)


type InputAddress = Index 784
type HiddenLayerAddress = Index 10
type OutputAddress = Index 10
type WeightAddr = Index 7940
type BiasAddr = Index 20
type InFirstLayer = Bool

data NetworkState = FirstLayer (InputAddress, HiddenLayerAddress)
                  | SecondLayer (HiddenLayerAddress, OutputAddress)
                  | Waiting
                  deriving (Generic, NFDataX, Show)

type HexDigit = BitVector 7


-- A 50 MHz clock (period=20,000 ps)
createDomain vSystem{vName="Dom50MHz", vPeriod=20000}
-- A 20 MHz clock (period=50,000 ps)
createDomain vSystem{vName="Dom20MHz", vPeriod=50000}


-- {-# ANN topEntity
--   (Synthesize
--     { t_name   = "neuralNetwork"
--     , t_inputs = [ PortName "CLOCK_50"
--                  , PortName "KEY0"
--                  ]
--     , t_output = PortProduct "" [PortName "LEDR", PortName "HEX0"]
--     })#-}

-- topEntity
--   :: Clock Dom50MHz
--       `Annotate` 'StringAttr "chip_pin" "AF14"
--   -> Signal Dom50MHz Bool
--       `Annotate` 'StringAttr "chip_pin" "AJ4"
--   -> Signal Dom50MHz (BitVector 10, HexDigit)
-- topEntity clk rst = exposeClockResetEnable go clk (unsafeFromLowPolarity rst) enableGen
--  where
--   go :: HiddenClockResetEnable Dom50MHz
--     => Signal Dom50MHz (BitVector 10, HexDigit)
--   go = bundle (led_out, hex_out)
--     where
--       led_out = pack <$> pwm10 nn_output
--       hex_out = complement . toSevenSegment . elemMax <$> nn_output
--       nn_output = neuralNetwork (pure Nothing)

type LastPixelFlag = Bool

neuralNetwork
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (InputAddress, NNParam))
  -> Signal dom (Vec 10 NNParam)
neuralNetwork inputWriter = outputVec
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

    weight    = unpack <$> blockRamBlob weightBlob weightAddr (pure Nothing)
    bias      = unpack <$> blockRamBlob biasBlob biasAddr (pure Nothing)
    maybeBias = mux nodeBegin' (fmap Just bias) (pure Nothing)

    inputVal  = blockRam inputImage inpAddr inputWriter
    hiddenVal = blockRam (replicate d10 0) hiddenAddr hiddenWriter
    maInput   = mux inpLayer' inputVal hiddenVal
    ma        = mealy mac 0 (bundle (maInput, weight, maybeBias))

    hiddenWriter = mux (nodeBegin' .&&. inpLayer'') (Just <$> bundle (hiddenAddr'', relu <$> ma)) (pure Nothing)
    outputVec = mealy nodeWriter (repeat 0, repeat 0) (bundle (nodeBegin', inpLayer'', outAddr'', ma))


weightBlob = $(memBlobTH Nothing weightsList)
biasBlob  = $(memBlobTH Nothing biasesList)

nodeWriter
  :: (Vec 10 NNParam, Vec 10 NNParam)
  -> (NewNodeFlag, InFirstLayer, OutputAddress, NNParam)
  -> ((Vec 10 NNParam, Vec 10 NNParam), Vec 10 NNParam)
nodeWriter (current, lockedOutput) (nodeBegin, inFirstLayer, outAddr, nodeValue)
  | nodeBegin && not inFirstLayer && outAddr == maxBound = ((outputVec, outputVec), outputVec)
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
      WeightAddr,
      BiasAddr
  )
stateToAddr state = case state of
  FirstLayer (inp, node)
    -> (True, inp, node, 0, wAddr, resize node)
    where
      wAddr = resize inp + (784 * resize node)
  SecondLayer (node, out)
    -> (False, 0, node, out, wAddr, bAddr)
    where
      wAddr = resize node + (7840 + 10 * resize out)
      bAddr = 10 + resize out
  Waiting
    -> (False, 0, 0, 0, 0, 0)
--  where
--   undefinedAddr = deepErrorX "stateToAddr: address is undefined."


type NewNodeFlag = Bool

stateMachine
  :: NetworkState         -- Current state
  -> Bool
  -> (NetworkState, NewNodeFlag) -- (New state, new node flag)
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


mac :: NNParam -> (NNParam, NNParam, Maybe NNParam) -> (NNParam, NNParam)
mac acc (x, y, newAcc)
  | isJust newAcc = (ma (fromJust newAcc) x y, acc)
  | otherwise     = (ma acc x y, acc)
  where
    ma a b c = a + b * c


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
