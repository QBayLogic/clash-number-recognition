# Clash - Number Recognition: Python helper scripts
This directory contains 5 Python scripts, each with their own purpose.
## [neural_network.py](neural_network.py)
This script uses Tensorflow to build and train a neural network. It will then write all parameters (weights and biases) to text files, so they can be exported.
It can also be used to test the model using external images in the form of Numpy arrays. During testing, these arrays were extracted from the FPGA using the Integrated Logic Analyser (ILA).
## [bayer2rgb.py](bayer2rgb.py)
This script serves two purposes: first, it is used to compare images throughout various steps of the preprocessing. These steps were performed identically to the Clash program. Second, it can be used to create a 'pixel stream' from a given image. This image is assumed bo be 480x640 pixels in 8-bit RGB format. The pixel stream simulates a stream of data which would be received from the D8M camera module for the given image. This text file can then be used for Clash simulation.
## [mnist_image_extraction.py](mnist_image_extraction.py):
Extract one or more images from the MNIST test set and save them as both Numpy arrays and as images. The images can be printed to test the Clash Number Recognition Demo. The Numpy arrays can be used as golden reference.
## [signal_tap_processing.py](signal_tap_processing.py):
Uses the golden references from [mnist_image_extraction.py](mnist_image_extraction.py), and the data extracted using Quartus Signal Tap to compare the two images. Used to improve the quality of the image processing.