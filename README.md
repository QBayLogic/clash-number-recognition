# Clash - Number recogntion

This repository contains the source code of a handwritten number recognition 
circuit. It uses a pre-trained model to perform the evaluation of the neural
network. 

The target platform during development is the 
[Terasic DE10-Standard](https://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&No=1081),
which features an Intel Cyclone V. The [camera interface](src/NumberRecognition/CameraInterface.hs)
has been made specifically for use with the 
[Terasic D8M-GPIO](https://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&No=1011).

This number recognition circuit is meant to be used alongside the a Terasic demo.
This demo, meant for the DE1-SoC, uses the D8M module to display the captured 
image on the connected VGA monitor. 


## Neural Network
The Clash Number Recognition Demo is able to create a neural network on an FPGA
when given its parameters (weights and biases) and model shape. In its current 
state, the model is trained in Python using Tensorflow. The model is a 
feedforward neural network with 1 hidden layer. The input consists of 784 nodes 
(28*28 pixels), the hidden layer of 100 nodes and the output layer of 10 nodes. 
The model is trained using `Float32`, but the parameters are interpreted by 
Clash as `SFixed 8 8`. For more information on the model, see 
[the Python README](python/README.md).


# Usage
## Python: 
  - Train a (feedforward) neural network
  - Export the parameters (weights and biases)
  - Add `weights.dat` and `biases.dat` to `src/NumberRecognition/`
## Clash:
  - Change the following parameters:
    `HPixelCount`, `HiddenNodes`, `OutputNodes` in [NeuralNetwork.hs](src/NumberRecognition/NeuralNetwork.hs)
  - Compile to Verilog using:
  ```bash
  cabal run -- clash --verilog NumberRecognition.VGAOverlay
  cabal run -- clash --verilog NumberRecognition.GreyBuffer
  cabal run -- clash --verilog NumberRecognition.NumberRecognitionDemo
  ```
## Modify Terasic demo:
  - Used demo: `DE1_SOC_D8M_LB_RTL` by Terasic
  - Modified to work on `DE10-Standard` (unsure of modifications, request source 
  code from developers)
  - Add all Verilog (`*.v`) files to the project
  - Create 3 new components in the projects topEntity (`DE10_STANDARD_D8M_LB_RTL.v`):
      * NumberRecognitionDemo
      * GreyBuffer
      * OutputOverlay
  - Connect the components to the existing components in the demo (*TODO: add 
  diagram*)
## Synthesise the design
Synthesise the design with your prefered synthesis tool. Since this project
targets a Cyclone V, Quartus was used during development.
      

# Problems
The neural network works exactly as expected. Given the same input (image from 
MNIST), the Python Tensorflow model, Haskell simulation and Clash design on the 
DE10-Standard produce the same outputs. The Python Tensorflow-trained model 
has a 97.8% accuracy on the MNIST test set.

However, the image preprocessing needs to be improved. The images received from
the D8M camera module are cropped, greyscaled and downsampled to produce the
input for the neural network. The cropping is hardcoded, and therefore the 
number needs to be exactly centered in the frame. In practice, it is difficult
to point the camera with such precision. 

Furthermore, the greyscaled images have a lower contrast than the training 
images from the MNIST database. Increasing this contrast, or even thresholding,
produces artifacts in the images. For example, the hole in the number 9 often
disappears when thresholding.

Due to these issues, the system has an accuracy of roughly 50% on printed images
from the MNIST test set.

# Proposed improvements
To improve the accuracy of the neural network, several improvments can be made:
1. Crop to a larger part of the image. Then, find the center of mass of the 
number and crop further such that the number is centered in the frame. (For this 
to work, assume that only one number is shown in the frame after the first crop)
2. Improve the greyscaling method (now approximates the Luminocity Algorithm) or 
find another method to improve the contrast of the image.
3. Although not a problem for the current state of the project, QBayLogic wants
a generalized solution to produce neural networks on hardware. Currently, the 
number of nodes in the hidden layer can be modified. Since the calculations 
(multiply accumulate) is not parallelised, increasing the number of nodes also
increases the time to evaluate the neural network. The number of nodes in the
input and output layer can also be modified, but this behaviour has not yet been
tested.
  The generalised approach could feature:
    - Variable number of nodes for each layer
    - Variable number of layers
    - Interface from which to extract this model to remove need for manual 
    changes
    - Parallelise the evaluation of the neural network
    - Analyse model to verify whether the configuration would fit