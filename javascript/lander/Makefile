# $< is input file
# $@ is output file

.PHONY: all

all: lander.js

%.js: %.org
  # Tangle
	emacs --quick --batch --file $< --funcall org-babel-tangle
  # Convert links to relative path
	sed --in-place --expression 's/\[\[file:\([^\/]\+\/\)*/\[\[file:/g' $@
