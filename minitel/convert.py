from PIL import Image

# Take a 80x72 grayscale image and convert to the
# graphics mode of Minitel

MINITEL_WIDTH = 40
MINITEL_HEIGHT = 24             # First line is reserved

palette = [0, 102, 128, 153, 179, 204, 230, 255]

def pixel_at(pixels, x, y, width):
    return pixels[y * width + x]

def quantize(p, palette):
    i = 0
    while p > ((palette[i] + palette[i+1])/2):
        ++i
    return i

if __name__ == "__main__":
    image_path = "img/vangogh.png"

    img = Image.open(image_path)
    img = img.resize((80, 72), Image.BICUBIC) # Resize to wanted resolution
    img = img.convert(mode="L")               # Grayscale
    img = img.quantize(colors=8)              # Quantize to 8 colors

    #palette = Image.open("img/palette.png")
    #img = img.quantize(palette=palette, colors=8)    # Quantize to 8 colors

    pixels = img.getdata()
    width, height = img.size

    # First, quantize the pixels to 3 bits, using the non-linear palette
    # of the Minitel
    #pixels_3bits = [ quantize(p, palette) for p in pixels ]
    #pixels_3bits = pixels

    pixels_3bits = [ 7 - p for p in pixels ]

    # Then, convert each block of 2x3 pixels of the Minitel screen into the
    # appropriate graphics character

    graphics = []

    for block_y in range(0, MINITEL_HEIGHT):
        for block_x in range(0, MINITEL_WIDTH):
            #print(pixel_at(pixels_3bits, block_x*2, block_y*3, width))

            # Get the 6 pixels for this block
            subpixels = [pixel_at(pixels_3bits, block_x*2  , block_y*3,   width),
                         pixel_at(pixels_3bits, block_x*2+1, block_y*3,   width),
                         pixel_at(pixels_3bits, block_x*2 ,  block_y*3+1, width),
                         pixel_at(pixels_3bits, block_x*2+1, block_y*3+1, width),
                         pixel_at(pixels_3bits, block_x*2 ,  block_y*3+2, width),
                         pixel_at(pixels_3bits, block_x*2+1, block_y*3+2, width)]

            # Compute the mean
            subpixels_mean = sum(subpixels) / len(subpixels)

            # Quantize to 1 bit to determine the pattern, based on the mean
            subpixels_1bit = [ 1 if p >= subpixels_mean else 0 for p in subpixels ]
            pattern = 32 + sum([ subpixels_1bit[i] << i for i in range(0, len(subpixels_1bit)) ])

            # Determine the background color
            bg_values = []
            for i in range(0, len(subpixels_1bit)):
                if subpixels_1bit[i] == 0:
                    bg_values.append(subpixels[i])
            if len(bg_values) > 0:
                bg_color = round(sum(bg_values) / len(bg_values))
            else:
                bg_color = 0

            # Determine the foreground color
            fg_values = []
            for i in range(0, len(subpixels_1bit)):
                if subpixels_1bit[i] == 1:
                    fg_values.append(subpixels[i])
            if len(fg_values) > 0:
                fg_color = round(sum(fg_values) / len(fg_values))
            else:
                fg_color = 0

            graphics.append((bg_color, fg_color, pattern))


    #print(len(graphics))
    graphics_str = [ "{" + str(fg) + "," + str(bg) + "," + str(pattern) + "}" for (fg, bg, pattern) in graphics ]
    print("byte c[][3] = {" + ','.join(graphics_str) + "};")
