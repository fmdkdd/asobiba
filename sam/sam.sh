#!/bin/sh

# Extracts text from game screenshots.
# Requires imagemagick and tesseract.

# Will write $screen.cropped (the OCR image) and $screen.txt (the extracted text).
screen=$1
cropped=${screen}.cropped
text=${screen}                  # tesseract appends .txt

# Crop and resize.  We only care about the chat area in the lower-left corner.
# Catmull-Rom interpolation yields best results as it enhances contrast.  600%
# resize is a good (empiric) value for WoW screenshots.
convert ${screen} -crop 600x340+25+700 -interpolate catrom -interpolative-resize 600% ${cropped}

# Then OCR using tesseract.  Page segmentation works best here as a single block
# of text (6).
tesseract ${cropped} ${text} -l fra -psm 6
