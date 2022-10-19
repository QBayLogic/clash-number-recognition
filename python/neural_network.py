import math
import os
import tensorflow as tf
import numpy as np
from fxpmath import Fxp


input_shape = (28, 28)
input_nodes = np.prod(input_shape)
hidden_nodes = 100
output_nodes = 10


def write_params_memfile(x1, x2, name: str, dtype=None):
    """Write the given biases to a file"""
    if dtype == None:
        xmax = np.max((np.abs(x1).max(), np.abs(x2).max()))
        high_bits = math.ceil(math.log(xmax, 2)) + 1
        dtype = f"S{high_bits}.8"
    print(f"{name} is formatted as: {dtype}")

    xs = np.concatenate((x1.flatten('F'), x2.flatten('F')), axis=0)
    x_str = "\n".join(f"{Fxp(x, dtype=dtype).bin()}" for x in xs)
    fp = f"nn_params/{name}.dat"
    f = open(fp, 'w')
    f.write(x_str)
    f.close()
    print(f"Wrote {name} to {fp}")
    return dtype


def write_config(wtype, btype):
    """Generate a Haskell file with configuration data"""
    s = (
        f""
    )

    s = (
        "module NumberRecognition.NNConfig where\n\n"
        "import Clash.Prelude\n\n"
    )
    s += (
        "-- Nodes per layer in the neural network\n"
        f"type HPixelCount = {input_shape[0]}\n"
        f"type InputNodes  = {np.prod(input_shape)}\n"
        f"type HiddenNodes = {hidden_nodes}\n"
        f"type OutputNodes = {output_nodes}\n\n"
    )
    w_int, w_frac = wtype[1:].split('.')
    b_int, b_frac = btype[1:].split('.')
    s += (
        "-- Types of the weights and biases parameters (binary encoded in .dat)\n"
        f"type WeightType = SFixed {w_int} {w_frac}\n"
        f"type BiasType   = SFixed {b_int} {b_frac}\n"
    )
    filepath = f"nn_params/NNConfig.hs"
    f = open(filepath, 'w')
    f.write(s)
    f.close()
    print(f"Wrote neural network configuration to {filepath}")


def evaluate_image(img):
    """Evaluate a given image using the neural network model"""
    dtype = 'S8.8'
    x = Fxp(img.flatten(), dtype=dtype)

    h = np.zeros(hidden_nodes, dtype=np.float32)
    for i in range(hidden_nodes):
        h[i] = np.dot(x, Fxp(w1[:, i], dtype=dtype))
        h[i] += Fxp(b1[i], dtype=dtype)

    h[h < 0] = 0
    h = Fxp(h, dtype=dtype)
    o = np.zeros(output_nodes, dtype=np.float32)
    for i in range(output_nodes):
        o[i] = np.dot(h, Fxp(w2[:, i], dtype=dtype))
        o[i] += Fxp(b2[i], dtype=dtype)
    o = Fxp(o, dtype=dtype)
    return o


mnist = tf.keras.datasets.mnist
(train_images, train_labels), (test_images, test_labels) = mnist.load_data()

# Converting image pixel values to [0,1) (255 does not map to 1)
train_images = train_images / 256.0
test_images = test_images / 256.0

# Converting labels to one-hot encoded vectors
train_labels_onehot = tf.keras.utils.to_categorical(train_labels)
test_labels_onehot = tf.keras.utils.to_categorical(test_labels)

# Setup checkpoint callback for reuse of trained model
checkpoint_path = "training/cp-{epoch:04d}.ckpt"
checkpoint_dir = os.path.dirname(checkpoint_path)
cp_callback = tf.keras.callbacks.ModelCheckpoint(
    filepath=checkpoint_path,
    save_weights_only=True,
    verbose=1
)

# Using Sequential() to build layers one after another
model = tf.keras.Sequential([
    # Flatten layer that converts 2D images to 1D vector
    tf.keras.layers.Flatten(input_shape=input_shape),
    # Hidden layer with 100 units and ReLU activation
    tf.keras.layers.Dense(units=hidden_nodes, activation='relu'),
    # Output layer with 10 units for 10 classes and softmax activation
    tf.keras.layers.Dense(units=output_nodes, activation='softmax')
])

model.compile(
    loss='categorical_crossentropy',
    optimizer='adam',
    metrics=['accuracy']
)

# Print a summary of the model with the layer types, output shapes and number of parameters
model.summary()

# If a checkpoint can be found, load it, otherwise relearn the model
latest = tf.train.latest_checkpoint(checkpoint_dir)
try:
    model.load_weights(latest)
    print(f"Loaded existing model, evaluating...")
    model.evaluate(test_images, test_labels_onehot, verbose=2)
except AttributeError:
    print(f"No existing model found, retraining model...")
    # Training the neural network, using callbacks for intermediate saving
    history = model.fit(
        x=train_images,
        y=train_labels_onehot,
        epochs=30,
        validation_data=(test_images, test_labels_onehot),
        callbacks=[cp_callback]
    )

# Write the parameters to a file
w1, b1, w2, b2 = model.get_weights()
weights_dtype = write_params_memfile(w1, w2, "weights")
biases_dtype = write_params_memfile(b1, b2, "biases")

write_config(weights_dtype, biases_dtype)

# Test the model on an input other than the MNIST testing set, e.g. data aquired
# using Quartus Signal Tap.
# print(f"Ref nr  Detected  Reconstructed Threshold")
# for label in range(10):
#     ref_img = np.asarray(
#         [np.load(f"data/Numbers/reference/arrays/nr{label}.npy") / 256.0])
#     reconstructed = np.asarray(
#         [np.load(f"data/Numbers/reconstructed/arrays/nr{label}.npy") / 256.0])
#     threshold = np.asarray(
#         [np.load(f"data/Numbers/reconstructed/arrays/nr{label}_hc.npy") / 256.0])

#     ref_out = evaluate_image(ref_img.flatten())
#     reconstr_out = evaluate_image(reconstructed.flatten())
#     threshold_out = evaluate_image(threshold.flatten())

#     print(f"{label}\t{ref_out.argmax()}\t{reconstr_out.argmax()}\t{threshold_out.argmax()}")

# print(f"\nFor input image which shows the number {label}:")
# # print(f"\nHidden layer nodes:\n{np.array2string(h.astype(np.float32), separator=', ')}")
# print(f"Output:\n{np.array2string(o.astype(np.float32), separator=', ')}")
# print(f"Detected number : {o.argmax()}")
