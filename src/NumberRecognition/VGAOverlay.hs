{-# LANGUAGE FlexibleContexts #-}
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
  , numberOverlay
  )
where

import Clash.Prelude

import NumberRecognition.CameraInterface (xEnd, xStart, yEnd, yStart)
import NumberRecognition.NeuralNetwork (PxVal)
import NumberRecognition.NumberLookup (NumHeight, NumWidth, numLUT)

{-|
  The DE10_STANDARD_D8M_LB_RTL demo by Terasic uses different counters for 
  coordinate position; these start counting once VSync starts, instead of when
  VSync /and/ HSync are high. Therefore, these values are higher than 
  'YCounter' and 'XCounter' used in this Clash project. Since the VGA component
  from the demo is used, these two counters are needed for the VGA overlay.
-}

type VSyncTotal = 526
type HSyncTotal = 793
type VOffset = 45
type HOffset = 152

type VCounter = Index VSyncTotal
type HCounter = Index HSyncTotal



{-# ANN topEntity
    (Synthesize
        { t_name = "VGAOverlay"
        , t_inputs = [ PortName "Y",
                       PortName "X",
                       PortName "R",
                       PortName "G",
                       PortName "B",
                       PortName "Grey",
                       PortName "Number"
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
  -- -- ^ Greyscaled pixel value
  -> Index 10
  -- ^ Detected number
  -> (PxVal, PxVal, PxVal)
  -- ^ RGB pixel values
topEntity v h r g b grey number = numberOverlay v h r' g' b' number
  where
    (r', g', b') = outputOverlay v h r g b grey



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
  | inFrame   = (grey, grey, grey)
  | otherwise = (shiftR r 1, shiftR g 1, shiftR b 1)
 where
  inFrame =
    hCount >= (resize xStart + (natToNum @HOffset)) &&
    hCount < (resize xEnd + (natToNum @HOffset)) &&
    vCount >= (resize yStart + (natToNum @VOffset)) &&
    vCount < (resize yEnd + (natToNum @VOffset))
{-# NOINLINE outputOverlay #-}


type OverlayAddr = Index (NumHeight * 10)
type Scaling = 4  -- Must be power of 2
type YOverlayStart = VSyncTotal - NumHeight * Scaling
type XOverlayStart = HSyncTotal - NumWidth * Scaling


numberOverlay
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
  -> Index 10
  -- ^ Detected number
  -> (PxVal, PxVal, PxVal)
  -- ^ RGB pixel values
numberOverlay vCount hCount r g b number
  | inFrame   = (out, out, out)
  | otherwise = (r, g, b)
  where
    inFrame = inFrameY && inFrameX
      where
        inFrameY =  vCount >= natToNum @YOverlayStart && 
                    vCount <  natToNum @(YOverlayStart + NumHeight * Scaling)
        inFrameX =  hCount >= natToNum @XOverlayStart && 
                    hCount <  natToNum @(XOverlayStart + NumWidth * Scaling)
    y = satSub SatBound vCount (natToNum @YOverlayStart)
    x = satSub SatBound hCount (natToNum @XOverlayStart)

    readAddr :: OverlayAddr
    readAddr = resize number `shiftL` (natToNum @(CLog 2 NumHeight)) + resize y'
      where
        y' :: Index NumHeight
        y'  = resize . flip shiftR (natToNum @(CLog 2 Scaling)) $ y

    bitAddr :: Index NumWidth
    bitAddr = resize . flip shiftR (natToNum @(CLog 2 Scaling)) $ x

    overlay = numLUT !! readAddr
    active = bitToBool (overlay ! bitAddr)
    out = if active then maxBound else minBound
{-# NOINLINE numberOverlay #-}
