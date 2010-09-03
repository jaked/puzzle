JS=puzzle.js

all:
	ocamlbuild $(JS); ln -s _build/$(JS) .

clean:
	ocamlbuild -clean
	rm -f $(JS)

github:
	cp index.html style.css $(JS) ../puzzle.gh-pages
