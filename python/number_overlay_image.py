import numpy as np
from PIL import Image, ImageDraw, ImageFont


def bools2bit_string(lst):
    return "0b" + ''.join(['1' if x else '0' for x in lst]) + "\n"


def arr2str(arr, n):
    """Construct a Clash-style Vector from a given array of booleans"""
    h, w = arr.shape[:2]
    header = f"n{n}Overlay :: Vec {h} (BitVector {w})\nn{n}Overlay = \n     "
    s = "  :> ".join([bools2bit_string(row) for row in arr])

    f = open("numbers/nOverlayLookup.hs", 'w')
    f.write(header + s)
    f.close()


def arrays2string(arr):
    """Construct a single Clash-style Vector from a 3D array of booleans."""
    n, h, w = arr.shape[:3]
    flipped = np.flip(arr, axis=2)
    s = f"module NumberRecognition.NumberLookup\n"\
        f"  ( -- * Types\n    NumHeight\n  , NumWidth\n" \
        f"  -- * Constants\n  , numLUT\n  )\nwhere\n\n" \
        f"import Clash.Prelude\n\n"\
        f"type NumHeight = {h}\n" \
        f"type NumWidth = {w}\n\n" \
        f"numLUT :: Vec (NumHeight * 10) (BitVector NumWidth)\nnumLUT =\n"
    for i, num in enumerate(flipped):
        num_header = f"  -- Number {i}\n"
        if i != 0:
            num_header += "  :> "
        else: 
            num_header += "     "
        num_str = "  :> ".join([bools2bit_string(row) for row in num])
        s += (num_header + num_str)
    s += "  :> Nil"
    return s


def draw_number(n, save_image=False):
    img = Image.new('1', (width, height), color='white')
    img_draw = ImageDraw.Draw(img)
    img_draw.text((width//2, height//2),
                  f"{n}", font=font, anchor='mm', align='center')
    if save_image:
        img.save(f"numbers/img/overlay_{n}.png")


def get_number_array(n):
    img = Image.new('1', (width, height), color='white')
    img_draw = ImageDraw.Draw(img)
    img_draw.text((width//2, height//2),
                  f"{n}", font=font, anchor='mm', align='center')
    return np.asarray(img, dtype=bool)


width, height = 16, 16
font = ImageFont.truetype(
    "/home/hidde/.fonts/droid-sans-mono.regular.ttf", size=height)

nums = np.array([get_number_array(n) for n in range(10)])

f = open("numbers/NumberLookup.hs", "w")
f.write(arrays2string(nums))
f.close()
