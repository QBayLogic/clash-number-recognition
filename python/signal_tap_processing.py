import numpy as np
import pandas as pd
from PIL import Image

bits2bool = lambda a: bool(int(a)) if a.isnumeric() else False
bits2num = lambda s: int(s, 2) if s.isnumeric() else 0

conv = {"MIPI_PIXEL_VS": bits2bool,
        "MIPI_PIXEL_HS": bits2bool,
        "GREYBUFFER_START": bits2bool,
        "GREYBUFFER_OUTPUT": bits2num,
        }

for nr in range(10):
  st = pd.read_csv(f"data/Numbers/reconstructed/signaltap/nr{nr}.csv",
                  usecols=list(conv.keys()),
                  skipinitialspace=True,
                  converters=conv,
                  skiprows=4,
                  header=0
                  )
  st.rename(columns={"MIPI_PIXEL_VS": "VS",
                    "MIPI_PIXEL_HS": "HS",
                    "GREYBUFFER_START": "BufferStart",
                    "GREYBUFFER_OUTPUT": "GreyBuffered",
                    },
            inplace=True
            )


  active = st.drop(st.index[st["VS"] == False])
  active.drop(["VS"], axis=1, inplace=True)
  active.drop(["HS"], axis=1, inplace=True)

  active["BufferNumber"] = (~active["BufferStart"].shift(1, fill_value=False) & active["BufferStart"]).cumsum()
  frame = active.loc[active["BufferNumber"] == 1]
  frame = frame.drop(["BufferNumber", "BufferStart"], axis=1)

  img = frame.to_numpy().astype(np.uint8)
  img = img.reshape(((28,28)))

  # - Increase the contrast of the image
  contrast_factor = 2
  img_hc = img.copy()
  img_hc[img_hc > (256//contrast_factor)-1] = 255
  img_hc[img_hc <= (265//contrast_factor)] * contrast_factor

  np.save(f"data/Numbers/reconstructed/arrays/nr{nr}.npy", img)
  np.save(f"data/Numbers/reconstructed/arrays/nr{nr}_hc.npy", img_hc)

  img_st = Image.fromarray(img, mode="L")
  img_st.save(f"data/Numbers/reconstructed/images/nr{nr}.png")

  img_st_hc = Image.fromarray(img_hc, mode="L")
  img_st_hc.save(f"data/Numbers/reconstructed/images/nr{nr}_hc.png")


