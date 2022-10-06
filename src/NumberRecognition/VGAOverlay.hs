{-| 
  Copyright: (C) 2022, QBayLogic B.V.
  License:   see LICENSE
  
  This module is used to overlay part of the VGA output stream. It is to be used
  with the /DE10_STANDARD_D8M_LB_RTL/ demo by Terasic. In this demo, the 
  /OutputOverlay/ component is placed between the demo's /FOCUS_ADJ/ component
  and the ouput to the VGA monitor.
  It overlays a square on the image representing what the 
  [neural network]("NumberRecognition.NeuralNetwork") \'sees\'. Inside this square, the 
  pixels are greyscaled and downsampled, identically to how the neural network 
  receives its input. Outside the square the brightness of the RGB pixels is 
  slightly decreased, to empasize the square.
-}


module NumberRecognition.VGAOverlay
  ( -- * Types
    VCounter
  , HCounter
  -- * Functions
  , topEntity
  , outputOverlay
  )
where

import Clash.Prelude

import NumberRecognition.CameraInterface (xEnd, xStart, yEnd, yStart)
import NumberRecognition.NeuralNetwork (PxVal)

{-|
  The DE10_STANDARD_D8M_LB_RTL demo by Terasic uses different counters for 
  coordinate position; these start counting once VSync starts, instead of when
  VSync /and/ HSync are high. Therefore, these values are higher than 
  'YCounter' and 'XCounter' used in this Clash project. Since the VGA component
  from the demo is used, these two counters are needed for the VGA overlay.
-}
type VCounter = Index 526
type HCounter = Index 793

vOffset = 45 :: VCounter
hOffset = 152 :: HCounter

{-# ANN topEntity
    (Synthesize
        { t_name = "VGAOverlay"
        , t_inputs = [ PortName "Y",
                       PortName "X",
                       PortName "R",
                       PortName "G",
                       PortName "B",
                       PortName "Grey"
                     ]
        , t_output = PortProduct "" [ PortName "outR"
                      , PortName "outG"
                      , PortName "outB"
                      ]
        })#-}

topEntity 
  :: VCounter 
  -- ^ Vertical position of output pixel
  -> HCounter 
  -- ^ Horizontal position of output pixel
  -> PxVal 
  -- ^ Red pixel value
  -> PxVal 
  -- ^ Green pixel value
  -> PxVal 
  -- ^ Blue pixel value
  -> PxVal 
  -- ^ Greyscaled pixel value
  -> (PxVal, PxVal, PxVal)
  -- ^ RGB pixel values
topEntity = outputOverlay


-- | Replace the RGB values with its corresponding greyscaled value in the 
-- bounding box
--
-- Decreases the brightness of the RGB pixels outside the neural networks input
-- box, and shows this box as the greyscaled and downscaled view which the 
-- network receives as its input.
outputOverlay 
  :: VCounter 
  -- ^ Vertical position of output pixel
  -> HCounter 
  -- ^ Horizontal position of output pixel
  -> PxVal 
  -- ^ Red pixel value
  -> PxVal 
  -- ^ Green pixel value
  -> PxVal 
  -- ^ Blue pixel value
  -> PxVal 
  -- ^ Greyscaled pixel value
  -> (PxVal, PxVal, PxVal)
  -- ^ RGB pixel values
outputOverlay vCount hCount r g b grey
  | inFrame =   (grey,grey,grey)
  | otherwise = (shiftR r 1, shiftR g 1, shiftR b 1)
 where
  inFrame =
    hCount >= (resize xStart + hOffset) && hCount < (resize xEnd + hOffset) &&
    vCount >= (resize yStart + vOffset) && vCount < (resize yEnd + vOffset)
{-# NOINLINE outputOverlay #-}