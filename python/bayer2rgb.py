import os
import cv2
import numpy as np
from scipy.signal import convolve2d
import matplotlib
import matplotlib.pyplot as plt
matplotlib.use('TkAgg')


def create_pixel_stream(image):
    """
    Creates a stream of pixel data and its respective Vsync and Hsync values.
    Expects `image` to have a resolution of 480 by 640 with 8-bit pixels. These
    pixels are in a raw bayern pattern of BGGR.

    VS and HS values:
        Cycles  VS  HS
        400     0   0
        45*793  1   0
        152     1   0   }
        640     1   1   } repeat 480 times
        1       1   0
        152+640 0   0
    """
    if (image.shape[:2] != (480, 640)):
        return (None, None, None)
    px_stream = np.concatenate([
        np.zeros(400, dtype=np.uint8),
        np.pad(image, ((45, 1), (153, 0)), 'constant').flatten(),
        np.zeros(1, dtype=np.uint8)
    ])
    vs_stream = np.concatenate([
        np.zeros(400, dtype=bool),
        np.ones(45*793, dtype=bool),
        np.ones(480*(153+640), dtype=bool),
        np.ones(1, dtype=bool),
        np.zeros(1*(153+640), dtype=bool)
    ])
    hs_stream = np.concatenate([
        np.zeros(400, dtype=bool),
        np.zeros(45*793, dtype=bool),
        np.tile(np.concatenate(
            [np.zeros(153, dtype=bool), np.ones(640, dtype=bool)]), 480),
        np.zeros(1, dtype=bool),
        np.zeros(1*(153+640), dtype=bool)
    ])
    np.savetxt("nn_params/camera_sim/pixeldata.dat",
               px, fmt="%d", delimiter=',')
    np.savetxt("nn_params/camera_sim/vs.dat", vs, fmt="%1d", delimiter=",")
    np.savetxt("nn_params/camera_sim/hs.dat", hs, fmt="%1d", delimiter=",")
    txt = (f"Wrote pixel data and accompaning Vsync and Hsync"
           f"values to separate files of length {px.shape[0]:,d}")
    print(txt)


def plot_color_channels(image, reds, greens, blues, file_path=""):
    """Show the image and its 3 color channels"""
    fig, axes = plt.subplots(2, 2, figsize=(8, 8),
                             sharex=True, sharey=True,
                             num="Image color seperation")
    fig.set_tight_layout(True)
    axes[0, 0].set_xticks([])
    axes[0, 0].set_yticks([])

    axes[0, 0].imshow(image)
    axes[0, 0].title.set_text("Original")
    axes[0, 1].imshow(reds, cmap='Reds')
    axes[0, 1].title.set_text("Red")
    axes[1, 0].imshow(greens, cmap='Greens')
    axes[1, 0].title.set_text("Green")
    axes[1, 1].imshow(blues, cmap='Blues')
    axes[1, 1].title.set_text("Blue")
    if os.path.exists(os.path.dirname(file_path)):
        plt.savefig(file_path, bbox_inches='tight')
        print(f"Saved figure to: {file_path}")
    plt.show()


def plot_image_comparison(image_dict, file_path=""):
    """Draw all given figures horizontally."""
    n = len(image_dict)
    fig, axes = plt.subplots(1, n, figsize=(4*n, 4),  num="Image comparison")
    fig.set_tight_layout(True)
    for i, (title, image) in enumerate(image_dict.items()):
        clr = plt.cm.gray if len(image.shape) == 2 else None
        axes[i].imshow(image, cmap=clr)
        axes[i].title.set_text(title)
        axes[i].set_xticks([])
        axes[i].set_yticks([])
    if os.path.exists(os.path.dirname(file_path)):
        plt.savefig(file_path, bbox_inches='tight')
        print(f"Saved figure to: {file_path}")
    plt.show()


def bayer2rgb(image_bayer):
    """Convert an image with bayer pattern to an RGB image. The resulting
    image is also downsampled by a factor of 4.
    """
    height, width = image_bayer.shape[:2]
    bayer_pad = np.pad(image_bayer, ((0, 1), (1, 0)), 'constant')
    image_rgb = np.zeros((height//4, width//4, 3), dtype=np.uint8)

    image_rgb[:, :, 0] = bayer_pad[1::4, 2::4]
    image_rgb[:, :, 1] = np.mean(np.array([bayer_pad[1::4, 3::4],
                                             bayer_pad[2::4, 2::4]]),
                                   axis=0)
    image_rgb[:, :, 2] = bayer_pad[2::4, 3::4]
    return image_rgb


def bayer2gray(image_bayer):
    """Converts an bayer encoded imaged to a grayscaled image. Downsamples
    the result by a factor of 4."""
    idxs = np.arange(image_bayer.shape[0])
    idxs = idxs[(idxs % 4 == 1) | (idxs % 4 == 2)]
    skipped = np.take(np.take(image_bayer, idxs, axis=0), idxs, axis=1)
    red = lambda x: np.right_shift(x, 2) + np.right_shift(x, 4)
    green = lambda x:  np.right_shift(x, 2) + np.right_shift(x, 5)
    blue = lambda x :np.right_shift(x, 3)
    skipped[0::2, 0::2] = red(skipped[0::2, 0::2])
    skipped[0::2, 1::2] = green(skipped[0::2, 1::2])
    skipped[1::2, 0::2] = green(skipped[1::2, 0::2])
    skipped[1::2, 1::2] = blue(skipped[1::2, 1::2])

    grey = convolve2d(skipped, np.ones(
        (2, 2), dtype=np.uint8), 'same')[1::2, 1::2]
    return grey


def rgb2bayer(image, layered: bool = False):
    (height, width) = image.shape[:2]
    (b, g, r) = cv2.split(image)

    if layered:
        bayer_rgb = np.zeros((height, width, 3), np.uint8)
        bayer_rgb[0::2, 0::2, 2] = b[0::2, 0::2]   # top left
        bayer_rgb[0::2, 1::2, 1] = g[0::2, 1::2]   # top right
        bayer_rgb[1::2, 0::2, 1] = g[1::2, 0::2]   # bottom left
        bayer_rgb[1::2, 1::2, 0] = r[1::2, 1::2]   # bottom right
        return bayer_rgb
    else:
        bayer = np.empty((height, width), np.uint8)
        bayer[0::2, 0::2] = b[0::2, 0::2]   # top left
        bayer[0::2, 1::2] = g[0::2, 1::2]   # top right
        bayer[1::2, 0::2] = g[1::2, 0::2]   # bottom left
        bayer[1::2, 1::2] = r[1::2, 1::2]   # bottom right
        return bayer


def crop(image, new_shape):
    """
    Crops any given image with at least 2 dimensions to the new shape.
    Ignores dimensions other than the first 2.
    """
    h, w = image.shape[:2]
    h_new, w_new = new_shape[:2]
    left = (w - w_new) // 2
    top = (h - h_new) // 2
    cropped = image[top:top + h_new,
                    left:left + w_new]
    return cropped


def compare_sim_and_ref():
    clash_sim = np.loadtxt("nn_params/camera_sim/clash_sim.dat", np.uint8)
    clash_sim = clash_sim.reshape((28, 28))
    golden_ref = np.loadtxt("nn_params/camera_sim/golden_reference.dat", np.uint8)
    golden_ref = golden_ref.reshape((28, 28))

    fig, (ax0, ax1) = plt.subplots(
        1, 2, figsize=(8, 4), sharex=True, sharey=True)
    fig.set_tight_layout(True)
    ax0.imshow(golden_ref, cmap=plt.cm.gray)
    ax0.title.set_text("Golden reference")
    ax1.imshow(clash_sim, cmap=plt.cm.gray)
    ax1.title.set_text("Clash simulation")

    plt.show()


if __name__ == "__main__":
    """Load the image from given filepath to a Numpy array"""
    filepath = "data/images/Numbers/nr5_480p.png"
    # filepath = "data/images/Numbers/nr0_480p.png"
    img = cv2.imread(filepath)

    (height, width) = img.shape[:2]
    (b, g, r) = cv2.split(img)
    img_rgb = cv2.merge((r, g, b))

    bayer = rgb2bayer(img)
    bayer_layered = rgb2bayer(img, layered=True)
    bayer_layered_cropped = crop(bayer_layered, (112, 112))
    cropped = crop(bayer, (112, 112))
    reconstruced_rgb = bayer2rgb(cropped)
    gray = bayer2gray(cropped)
    rgb = cv2.cvtColor(crop(bayer, (112, 112)), cv2.COLOR_BayerBG2BGR)

    """Show the images"""
    img_dict = {
        "Original": img_rgb,
        "Bayer": bayer_layered,
        "Bayer cropped": bayer_layered_cropped,
        # "RGB OpenCV": rgb,
        "Reconstructed RGB": reconstruced_rgb,
        "grayscaled": gray
    }
    # plot_color_channels(img_rgb, r, g, b)
    fp = "data/images/bayer/rainbow_comparison.png"
    # plot_image_comparison(img_dict, file_path="")

    """Generate the pixel stream"""
    create_pixel_stream(bayer)

    """Save the golden reference output to a text file."""
    np.savetxt("nn_params/camera_sim/golden_reference.dat",
               gray.flatten(), fmt="%3d", delimiter=',')

    # compare_sim_and_ref()
