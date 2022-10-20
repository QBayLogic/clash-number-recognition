{-| 
  Copyright: (C) 2022, QBayLogic B.V.
  License:   see LICENSE

  The GreyBuffer module houses a single function and its corresponding 
  topentity. The 'greyBuffer' function stores one line greyscaled pixels. Its
  output can then be used to overlay the these greyscaled pixels over the VGA 
  output, thus showing what the neural network receives as an input.
-}

module NumberRecognition.GreyBuffer 
  (
    topEntity
  , greyBuffer
  )
where

import Clash.Prelude
import Data.Maybe (fromJust, isJust)

import NumberRecognition.CameraInterface (XCounter, XStart)
import NumberRecognition.NeuralNetwork (PxVal)
import NumberRecognition.NNConfig (HPixelCount, InputNodes)

createDomain vSystem{vName="Dom25MHz", vPeriod=40000}


{-# ANN topEntity
    (Synthesize
        { t_name = "GreyBuffer"
        , t_inputs = [ PortName "CLOCK_25",
                       PortName "RESET",
                       PortName "XCounter",
                       PortName "WriteGrey"
                     ]
        , t_output = PortName "outGrey"
        })#-}

topEntity 
  :: Clock Dom25MHz 
  -- ^ Clock
  -> Signal Dom25MHz Bool 
  -- ^ Reset
  -> Signal Dom25MHz XCounter 
  -- ^ Horizontal counter
  -> Signal Dom25MHz (Maybe (Index InputNodes, PxVal)) 
  -- ^ Write address and value of the linebuffer
  -> Signal Dom25MHz PxVal
  -- ^ Pixel value from linebuffer at the given X coordinate
topEntity clk rst =
  withClockResetEnable clk (unsafeFromLowPolarity rst) enableGen greyBuffer


-- | Linebuffer for greyscaled pixels
-- 
-- Writes given pixel values to the next place in the linebuffer. The returned
-- pixel value corresponds with the pixel on the output location on the screen.
greyBuffer ::
  HiddenClockResetEnable dom
  => Signal dom XCounter
  -- ^ Horizontal counter
  -> Signal dom (Maybe (Index InputNodes, PxVal))
  -- ^ Write address and value of the linebuffer
  -> Signal dom PxVal
  -- ^ Pixel value from linebuffer at the given X coordinate
greyBuffer xCount writeGrey = bundle readGrey
 where
  readGrey = blockRamU NoClearOnReset (SNat @HPixelCount) (const (deepErrorX "")) readAddr writeOp

  readAddr = (\x -> resize ((`shiftR` 2) (satSub SatError x (natToNum @XStart)))) <$> xCount
  writeAddr = register (0 :: Index HPixelCount) writeAddr'
  writeAddr' = mux (isJust <$> writeGrey) (satSucc SatWrap <$> writeAddr) writeAddr
  writeOp = mux
    (isJust <$> writeGrey)
    (Just <$> bundle (writeAddr, snd . fromJust <$> writeGrey))
    (pure Nothing)
{-# NOINLINE greyBuffer #-}