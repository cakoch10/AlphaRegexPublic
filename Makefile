main.native:
	ocamlbuild -use-ocamlfind main.native

clean:
	rm -fr _build main.native