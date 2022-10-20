{-|
  Copyright: (C) 2022, QBayLogic B.V.
  License:   see LICENSE

  The CameraInterface module provides an interface to process a given camera
  input. For this specific application, the connected camera module is the 
  Terasic D8M daughter board, which returns raw pixel data (10-bit) in a bayer 
  pattern (RGGB). Besides pixel data it also ouputs a Vertical and Horizontal 
  Sync value.

  The module is used to process this raw data from the camera module such that
  it can be used by the [neural network]("NumberRecognition.NeuralNetwork"). 
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK show-extensions, not-home #-}

module NumberRecognition.CameraInterface
  ( -- * Types
    XCounter
  , YCounter
  , PxDataRaw
  , HS
  , VS
  , XStart
  , YStart
  , Size
    -- * Data types
  , BayerState (IgnorePixel)
  -- * Functions
  , d8mProcessing
  , coordinateCounter
  , greyscale
  , bayerStateMachine
  )
where

import Clash.Prelude

import NumberRecognition.NeuralNetwork (PxVal, InputAddress)
import NumberRecognition.NNConfig (HPixelCount)


type HResolution = 640
type VResolution = 480

type YCounter = Index VResolution
type XCounter = Index HResolution
type PxDataRaw = BitVector 10

type VS = Bool
type HS = Bool
type SyncState = (VS, HS, YCounter, XCounter)

type LineBufferAddress = Index (HPixelCount * 2)
type Scaling = 4
type Size = HPixelCount * Scaling

type XStart = (HResolution - Size) `Div` 2
type YStart = (VResolution - Size) `Div` 2

{-|
  The 'BayerState' data type contains 7 states. Each has an input address and
  an address for the linebuffer.
-}
data BayerState = IgnorePixel     (InputAddress, LineBufferAddress)
                | StoreRed        (InputAddress, LineBufferAddress)
                | StoreGreen      (InputAddress, LineBufferAddress)
                | IgnorePixelEven (InputAddress, LineBufferAddress)
                | LoadRed         (InputAddress, LineBufferAddress)
                | LoadGreen       (InputAddress, LineBufferAddress)
                | CombinePixel    (InputAddress, LineBufferAddress)
                deriving (Generic, NFDataX, Show)

-- | Process the input from the camera module for neural network evaluation
--
-- Use the given coordinates of the input frame to crop, greyscale and 
-- downsample the image. For one given frame at the input, the output is 
-- 'InputNodes' times a Just value containing the write address and greyscale
-- pixel data.
d8mProcessing
  ::
  forall dom .
  HiddenClockResetEnable dom
  => Signal dom PxDataRaw
  -- ^ Raw pixel data
  -> Signal dom (YCounter, XCounter)
  -- ^ Vertical and Horizontal position of the input pixel
  -> Signal dom (Maybe (InputAddress, PxVal))
  -- ^ Processed pixel data and accompanying address
d8mProcessing pxD yx = bundle (stateToOutputWriter <$> state <*> pxGreyInverted)
  where
    pxD' = bitCoerce . resize . flip shiftR 2 <$> pxD
    state = register (IgnorePixel (0, 0)) (bayerStateMachine <$> state <*> yx)

    lineBuffer = blockRam @dom (replicate (SNat @(HPixelCount * 2)) 0) rdAddr writer
    lineBufferReg = register 0 lineBuffer
    pxReg = register 0 pxD'
    pxGreyInverted = xor maxBound <$> pxGrey
    pxGrey = greyscale <$> bundle (lineBufferReg, lineBuffer, pxReg, pxD')
    writer = stateToWriter <$> state <*> pxD'
    rdAddr = stateToReadAddress <$> state
{-# NOINLINE d8mProcessing #-}


-- | Greyscale the given bayer-encoded pixel
--
-- Greyscale using bit-shifts closest to luminosity algorithm. The input is 
-- assumed to be 4 pixel values with bayer encoding as:
--
-- >   R G
-- >   G B
-- 
greyscale
  :: (PxVal, PxVal, PxVal, PxVal)
  -- ^ Pixel values in order: Red, Green, Green, Blue
  -> PxVal
  -- ^ Greyscaled pixel value
greyscale (r, g0, g1, b) = grey
  where
    grey   = sum (zipWith shiftR rgb shifts)
    rgb    = r :> r :> g0 :> g0 :> g1 :> g1 :> b :> b :>Nil
    shifts = 2 :> 4 :> 2  :> 5  :> 2  :> 5  :> 3 :> 8 :> Nil
{-# NOINLINE greyscale #-}


-- | Statemachine for the 'BayerState'
bayerStateMachine
  :: BayerState
  -- ^ Current state
  -> (YCounter, XCounter)
  -- ^ Vertical and horizontal counter of pixel position of camera input
  -> BayerState
  -- ^ Updated state
bayerStateMachine state (y, x) = newState
  where
    newState = case state of
      IgnorePixel (outAddr, wrAddr)
        | inFrameY && inFrameX && lsby == 0b01 && lsbx == 0b00
        -> StoreRed (outAddr, wrAddr)
        | inFrameY && lsby == 0b10
        -> IgnorePixelEven (outAddr, 0)
        | y >= natToNum @(YStart + Size)
        -> IgnorePixel (0, 0)
        | otherwise
        -> state
      StoreRed (outAddr, wrAddr)
        -> StoreGreen (outAddr, satSucc SatBound wrAddr)
      StoreGreen (outAddr, wrAddr)
        -> IgnorePixel (outAddr, satSucc SatBound wrAddr)
      IgnorePixelEven (outAddr, rdAddr)
        | inFrameX && lsbx == 0b11
        -> LoadRed (outAddr, rdAddr)
        | lsby /= 0b10
        -> IgnorePixel (outAddr, 0)
        | otherwise
        -> state
      LoadRed (outAddr, rdAddr)
        -> LoadGreen (outAddr, satSucc SatBound rdAddr)
      LoadGreen (outAddr, rdAddr)
        -> CombinePixel (outAddr, satSucc SatBound rdAddr)
      CombinePixel (outAddr, rdAddr)
        -> IgnorePixelEven (succ outAddr, rdAddr)
      where
        lsby = resize (pack y) :: BitVector 2
        lsbx = resize (pack x) :: BitVector 2
        inFrameY = y >= natToNum @YStart && y < natToNum @(YStart + Size)
        inFrameX = x >= natToNum @XStart && x < natToNum @(XStart + Size)

-- | Derive an X and Y coordinate from the given Horizontal and Vertical Sync
--
-- Vertical Sync is high during an entire frame. Horizontal sync is only high
-- while the camera module outputs a horizontal line of the frame.
coordinateCounter
  :: SyncState
  -- ^ Current state (last VSync, HSync, Y and X values)
  -> (VS, HS)
  -- ^ Current Vertical and Horizontal Sync
  -> (SyncState, (YCounter, XCounter))
  -- ^ Updated state and coordinates of input pixel
coordinateCounter (pre_vs, pre_hs, y, x) (vs, hs) = ((vs, hs, y', x'), (y, x))
  where
    (y', x')
      | pre_vs && not vs = (0, 0)
      | pre_hs && not hs = (satSucc SatBound y, 0)
      | hs = (y, satSucc SatBound x)
      | otherwise = (y, x)
{-# NOINLINE coordinateCounter #-}


-- | Extracts the read address from the state
-- 
-- Only extract the address in states where the the linebuffer should be read. 
-- Returns an error when an address is requested while not in a reading state.
stateToReadAddress
  :: BayerState
  -- ^ Current state
  -> LineBufferAddress
  -- ^ Read address extracted from state
stateToReadAddress s = case s of
  IgnorePixelEven (_, rdAddr) -> rdAddr
  LoadRed         (_, rdAddr) -> rdAddr
  LoadGreen       (_, rdAddr) -> rdAddr
  CombinePixel    (_, rdAddr) -> rdAddr
  _ -> deepErrorX "stateToReadAddress: read address requested while not in reading state."

-- | Create a Just value for writing to a BlockRam
stateToWriter
  :: BayerState
  -- ^ Current state
  -> PxVal
  -- ^ RGGB pixel value
  -> Maybe (LineBufferAddress, PxVal)
  -- ^ Just address and value when in a writer state, Nothing otherwise
stateToWriter s p = case s of
  StoreRed   (_, wrAddr) -> Just (wrAddr, p)
  StoreGreen (_, wrAddr) -> Just (wrAddr, p)
  _ -> Nothing

-- | Create a Just value for writing to the output
stateToOutputWriter
  :: BayerState
  -- ^ Current state
  -> PxVal
  -- ^ Greyscaled pixel value
  -> Maybe (InputAddress, PxVal)
  -- ^ Just address and value when in state 'CombinePixel', Nothing otherwise
stateToOutputWriter s p = case s of
  CombinePixel (outAddr, _) -> Just (outAddr, p)
  _ -> Nothing
