{-|
  Copyright: (C) 2022, QBayLogic B.V.
  License:   see LICENSE

  This package contains the source code of a handwritten number recognition 
  circuit. It uses a pre-trained model to perform the evaluation of a neural
  network. 

  The target platform is the Terasic DE10-Standard, which features an Intel
  Cyclone V. The [camera interface]("NumberRecognition.CameraInterface") is made specifically
  for use with the Terasic D8M-GPIO.

  This number recognition circuit is meant to be used alongside the a Terasic 
  demo. This demo, meant for the DE1-SoC, uses the D8M module to display the 
  captured image on the connected VGA monitor. 
-}
module NumberRecognition where


import Clash.Prelude

main = _
