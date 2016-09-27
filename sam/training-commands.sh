# Following https://github.com/tesseract-ocr/tesseract/wiki/TrainingTesseract

# As close as possible to what will be input to tesseract.  I used the same font,
# eyeballed the font size, and used the same paper size.
text2image --leading=0 --xsize=1200 --ysize=600 --resolution=300 --ptsize=6 --text=training-text.txt --outputbase=fra.ArialNarrow.exp0 --font='Arial Condensed' --fonts_dir=./

# Then generate the TR file.  Using psm 6 leaves only 3 failures
tesseract -psm 6 -l fra fra.ArialNarrow.exp0.tif fra.ArialNarrow.exp0 box.train

# Then generate the unicharset
unicharset_extractor fra.ArialNarrow.exp0.box

# Then clustering
mftraining -F font_properties -U unicharset -O lang.unicharset fra.ArialNarrow.exp0.tr

cntraining fra.ArialNarrow.exp0.tr

# Then add prefix
cp shapetable wow.shapetable
cp inttemp wow.inttemp
cp pffmtable wow.pffmtable
cp unicharset wow.unicharset
cp normproto wow.normproto

# And combine
combine_tessdata wow.
