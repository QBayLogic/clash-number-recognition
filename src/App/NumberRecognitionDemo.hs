{-# LANGUAGE FlexibleContexts #-}
module App.NumberRecognitionDemo where

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes


import App.NeuralNetwork (HexDigit, Dom50MHz, neuralNetwork, toSevenSegment, elemMax)
import App.CameraInterface (PxDataRaw, VS, HS, d8mProcessing)


{-# ANN topEntity
    (Synthesize 
        { t_name = "NumberRecognitionDemo"
        , t_inputs = [ PortName "CLOCK_50"
                     , PortName "KEY0"
                     , PortName "PxDataRaw"
                     , PortName "VSYNC"
                     , PortName "HSYNC"
                     ]
        , t_output = PortName "HEX0"
        })#-}

topEntity
  :: Clock Dom50MHz
       `Annotate` 'StringAttr "chip_pin" "AF14"
  -> Signal Dom50MHz Bool
        `Annotate` 'StringAttr "chip_pin" "AJ4"
  -> Signal Dom50MHz PxDataRaw
  -> Signal Dom50MHz VS
  -> Signal Dom50MHz HS
  -> Signal Dom50MHz HexDigit
topEntity clk rst pxd vs hs = 
    exposeClockResetEnable go clk (unsafeFromLowPolarity rst) enableGen
  where
    go :: HiddenClockResetEnable Dom50MHz
       => Signal Dom50MHz HexDigit
    go = complement . toSevenSegment . elemMax <$> neuralNetwork (d8mProcessing pxd vs hs)