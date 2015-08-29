box.js: box.org
	emacs --quick --batch --file box.org --funcall org-babel-tangle
  # Convert links to relative path
	sed --in-place --expression 's/\[\[file:\([^\/]\+\/\)*/\[\[file:/g' box.js
