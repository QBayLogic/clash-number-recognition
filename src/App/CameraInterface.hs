{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module App.CameraInterface where

import Clash.Prelude

import App.NeuralNetwork (InputAddress)
import App.NNParamsList (NNParam)


type Xcounter = Index 640
type Ycounter = Index 480

type PxDataRaw = BitVector 10
type PxVal = Unsigned 8

type VS = Bool
type HS = Bool
type SyncState = (VS, HS, Ycounter, Xcounter)


type LineBufferAddress = Index 56 -- 28*2

xStart = 264 :: Xcounter  -- (640-112)/2
xEnd   = 375 :: Xcounter  -- 640 - xStart - 1
yStart = 184 :: Ycounter  -- (640-112)/2
yEnd   = 295 :: Ycounter  -- 480 - yStart - 1

data BayerState = IgnorePixel     (InputAddress, LineBufferAddress)
                | StoreRed        (InputAddress, LineBufferAddress)
                | StoreGreen      (InputAddress, LineBufferAddress)
                | IgnorePixelEven (InputAddress, LineBufferAddress)
                | LoadRed         (InputAddress, LineBufferAddress)
                | LoadGreen       (InputAddress, LineBufferAddress)
                | CombinePixel    (InputAddress, LineBufferAddress)
                deriving (Generic, NFDataX, Show)


d8mProcessing
  ::
  forall dom .
  HiddenClockResetEnable dom
  => Signal dom PxDataRaw
  -> Signal dom VS
  -> Signal dom HS
  -> Signal dom (Maybe (InputAddress, NNParam))
d8mProcessing pxD vs hs = bundle (stateToOutputWriter <$> state <*> pxGrey)
  where
    pxD' = bitCoerce . resize . flip shiftR 2 <$> pxD
    yx = mealy coordinateCounter (False, False, 0, 0) (bundle (vs, hs))
    state = register (IgnorePixel (0, 0)) (bayerStateMachine <$> state <*> yx)

    lineBuffer = blockRam @dom (replicate d56 0) rdAddr writer
    lineBufferReg = register 0 lineBuffer
    pxReg = register 0 pxD'
    pxGrey = greyscaleShiftingBayer <$> bundle (lineBufferReg, lineBuffer, pxReg, pxD')
    writer = stateToWriter <$> state <*> pxD'
    rdAddr = stateToReadAddress <$> state


-- Greyscaling using shifts closest to luminosity algorithm (second blue is ignored)
greyscaleShiftingBayer :: (PxVal, PxVal, PxVal, PxVal) -> PxVal
greyscaleShiftingBayer (r, g0, g1, b) = grey
  where
    grey   = sum (zipWith shiftR rgb shifts)
    rgb    = r :> r :> g0 :> g0 :> g1 :> g1 :> b :> Nil
    shifts = 2 :> 4 :> 2  :> 5  :> 2  :> 5  :> 3 :> Nil


bayerStateMachine
  :: BayerState
  -> (Ycounter, Xcounter)
  -> BayerState
bayerStateMachine state (y, x) = newState
  where
    newState = case state of
      IgnorePixel (outAddr, wrAddr)
        | yInFrame && xInFrame && lsby == 0b01 && lsbx == 0b00
        -> StoreRed (outAddr, wrAddr)
        | (yStart <= y && y < yEnd) && lsby == 0b10
        -> IgnorePixelEven (outAddr, 0)
        | y >= yEnd
        -> IgnorePixel (0, 0)
        | otherwise
        -> state
      StoreRed (outAddr, wrAddr)
        -> StoreGreen (outAddr, satSucc SatBound wrAddr)
      StoreGreen (outAddr, wrAddr)
        -> IgnorePixel (outAddr, satSucc SatBound wrAddr)
      IgnorePixelEven (outAddr, rdAddr)
        | xInFrame && lsbx == 0b11
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
        yInFrame = yStart <= y && y < yEnd
        xInFrame = (xStart - 1) <= x && x < xEnd


coordinateCounter ::
  SyncState ->
  (VS, HS) ->
  (SyncState, (Ycounter, Xcounter))
coordinateCounter (pre_vs, pre_hs, y, x) (vs, hs) = ((vs, hs, y', x'), (y, x))
  where
    (y', x')
      | pre_vs && not vs = (0, 0)
      | pre_hs && not hs = (satSucc SatBound y, 0)
      | hs = (y, satSucc SatBound x)
      | otherwise = (y, x)

stateToReadAddress :: BayerState -> LineBufferAddress
stateToReadAddress s = case s of
  IgnorePixelEven (_, rdAddr) -> rdAddr
  LoadRed         (_, rdAddr) -> rdAddr
  LoadGreen       (_, rdAddr) -> rdAddr
  CombinePixel    (_, rdAddr) -> rdAddr
  _ -> 0

stateToWriter :: BayerState -> PxVal -> Maybe (LineBufferAddress, PxVal)
stateToWriter s p = case s of
  StoreRed   (_, wrAddr) -> Just (wrAddr, p)
  StoreGreen (_, wrAddr) -> Just (wrAddr, p)
  _ -> Nothing

stateToOutputWriter :: BayerState -> PxVal -> Maybe (InputAddress, NNParam)
stateToOutputWriter s p = case s of
  CombinePixel (outAddr, _) -> Just (outAddr, toNNParam p)
  _ -> Nothing

toNNParam :: PxVal -> NNParam
toNNParam a = resizeF (unpack (zeroExtend (pack a)) :: SFixed 1 8)