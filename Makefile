.PHONY: all clean byte test repl utop

OCB_FLAGS = -tag bin_annot -use-menhir -use-ocamlfind -pkgs oUnit
OCB = ocamlbuild $(OCB_FLAGS)

all: infer

clean:
	$(OCB) -clean

infer:
	$(OCB) infer.byte

utop: all
	utop

test:
	$(OCB) test.byte && ./test.byte
