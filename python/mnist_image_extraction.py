import numpy as np
import tensorflow as tf
from PIL import Image


def resize(img, target_size):
    img_large = np.kron(img, np.ones((4,4), dtype=np.uint8))
    print(f"{img_large.shape = }")
    h_target, w_target = target_size
    h, w = img_large.shape[:2]
    top = (h_target - h) // 2
    left =  (w_target - w) // 2
    img_padded = np.pad(img_large, ((top, top), (left, left)), 'maximum')
    return img_padded


(train_images, train_labels), (test_images, test_labels) = tf.keras.datasets.mnist.load_data()

img_dict = dict()
j = 20
while len(img_dict.keys()) != 10:
    lab = test_labels[j]
    if lab not in img_dict:
        img_dict[lab] = test_images[j]
    j += 1

for label, img in img_dict.items():
    np.save(f"data/Numbers/reference/arrays/nr{label}.npy", img)
    im = Image.fromarray(img, mode='L')
    fp = f"data/Numbers/reference/images/nr{label}.png"
    im.save(fp)
