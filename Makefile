JS=puzzle.js

all:
	ocamlbuild $(JS); ln -s _build/$(JS) .

clean:
	ocamlbuild -clean
	rm -f $(JS)
