# $< is input file
# $@ is output file

.PHONY: all

all: box.js img/spatial-hash1.png img/spatial-hash2.png img/vec_length.png img/vec_perp.png img/vec_plus.png

%.js: %.org
  # Tangle
	emacs --quick --batch --file $< --funcall org-babel-tangle
  # Convert links to relative path
	sed --in-place --expression 's/\[\[file:\([^\/]\+\/\)*/\[\[file:/g' $@

img/%.png: img/%.ase
  # Flatten to PNG
	aseprite --batch $< --save-as $@
  # Scale with imagemagick
	convert $@ -filter point -resize 200% $@
