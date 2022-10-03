{-# LANGUAGE FlexibleContexts #-}
module App.NumberRecognitionDemo where

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes


import App.NeuralNetwork (InputAddress, PxVal, OutputVec, HexDigit, neuralNetwork, toSevenSegment, elemMax)
import App.CameraInterface (PxDataRaw, VS, HS, d8mProcessing, coordinateCounter, YCounter, XCounter)


-- A 25 MHz clock (period=40,000 ps)
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
                      PortName "HEX0",
                      PortName "WriteGrey",
                      PortProduct "" [
                        PortName "YCount",
                        PortName "XCount"
                      ]
        ]
        })#-}

topEntity
  :: Clock Dom25MHz
       `Annotate` 'StringAttr "chip_pin" "AF14"
  -> Signal Dom25MHz Bool
        `Annotate` 'StringAttr "chip_pin" "AJ4"
  -> Signal Dom25MHz PxDataRaw
  -> Signal Dom25MHz VS
  -> Signal Dom25MHz HS
  -> Signal Dom25MHz (HexDigit, Maybe (InputAddress, PxVal), (YCounter, XCounter))
topEntity clk rst pxd vs hs = bundle
  ( exposeClockResetEnable go clk reset enableGen
  , preProcessed
  , yx
  )
  where
    go :: HiddenClockResetEnable Dom25MHz
       => Signal Dom25MHz HexDigit
    go = displayHex <$> neuralNetwork preProcessed
    -- TODO Add delay to Pixel value pxd
    preProcessed = withClockResetEnable clk reset enableGen 
      d8mProcessing pxd yx
    yx = withClockResetEnable clk reset enableGen $
      mealy coordinateCounter (False, False, 0, 0) (bundle (vs, hs))
    reset = unsafeFromLowPolarity rst

{-# NOINLINE displayHex #-}
displayHex :: OutputVec -> HexDigit
displayHex = complement . toSevenSegment . elemMax