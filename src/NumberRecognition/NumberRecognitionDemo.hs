{-| 
  Copyright: (C) 2022, QBayLogic B.V.
  License:   see LICENSE

  The NumberRecognitionDemo module connects the 
  [camera interface]("NumberRecognition.CameraInterface") and the 
  [neural network]("NumberRecognition.NeuralNetwork") modules. The result is a component which
  receives its input from the D8M camera module, and outputs the recognised 
  number to a 7-segment display.
-}

{-# LANGUAGE FlexibleContexts #-}
module NumberRecognition.NumberRecognitionDemo
  (
    topEntity
  , displayHex
  )
where

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes


import NumberRecognition.NeuralNetwork (InputAddress, PxVal, OutputVec, HexDigit,
  neuralNetwork, toSevenSegment, elemMax, OutputNodes)
import NumberRecognition.CameraInterface (PxDataRaw, VS, HS, YCounter, XCounter, 
  d8mProcessing, coordinateCounter)


-- | A 25 MHz clock domain (period=40,000 ps)
createDomain vSystem{vName="Dom25MHz", vPeriod=40000}


{-# ANN topEntity
    (Synthesize
        { t_name = "NumberRecognitionDemo"
        , t_inputs = [ PortName "CLOCK_25"
                     , PortName "RESET_KEY"
                     , PortName "PIXELDATA"
                     , PortName "VSYNC"
                     , PortName "HSYNC"
                     ]
        , t_output = PortProduct "" [
                      PortProduct "" [
                        PortName "HEX",
                        PortName "Number"
                      ],
                      PortName "WriteGrey",
                      PortProduct "" [
                        PortName "YCount",
                        PortName "XCount"
                      ]
        ]
        })#-}
-- | TopEntity for the NumberRecognitionDemo
topEntity
  :: Clock Dom25MHz
       `Annotate` 'StringAttr "chip_pin" "AF14"
  -- ^ Clock
  -> Signal Dom25MHz Bool
        `Annotate` 'StringAttr "chip_pin" "AJ4"
  -- ^ Reset
  -> Signal Dom25MHz PxDataRaw
  -- ^ Raw pixel data
  -> Signal Dom25MHz VS
  -- ^ Vertical Sync
  -> Signal Dom25MHz HS
  -- ^ Horizontal Sync
  -> Signal Dom25MHz ((HexDigit, Index OutputNodes), Maybe (InputAddress, PxVal), (YCounter, XCounter))
  -- ^ BitVector for 7-segment display, preprocessing output, pixel coordinates
topEntity clk rst pxd vs hs = bundle
  ( exposeClockResetEnable go clk reset enableGen
  , preProcessed
  , yx
  )
  where
    go :: HiddenClockResetEnable Dom25MHz
       => Signal Dom25MHz (HexDigit, Index OutputNodes)
    go = displayHex <$> neuralNetwork preProcessed
    -- TODO: Add delay to Pixel value pxd
    -- pxd' = register 0 pxd
    preProcessed = withClockResetEnable clk reset enableGen 
      d8mProcessing pxd yx
    yx = withClockResetEnable clk reset enableGen $
      mealy coordinateCounter (False, False, 0, 0) (bundle (vs, hs))
    reset = unsafeFromLowPolarity rst

-- | Extract the index of the largest element and convert to a 'BitVector' to
-- show on a 7-segment display
displayHex :: OutputVec -> (HexDigit, Index OutputNodes)
displayHex inp = (hex nr, nr)
  where
    nr = elemMax inp
    hex = complement . toSevenSegment 
{-# NOINLINE displayHex #-}