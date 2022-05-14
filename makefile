all: evaluation expr miniml test_simple test_expr test_eval

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

test_simple: test_simple.ml 
	ocamlbuild -use-ocamlfind test_simple.byte

test_expr: test_expr.ml 
	ocamlbuild -use-ocamlfind test_expr.byte

test_eval: test_eval.ml 
	ocamlbuild -use-ocamlfind test_eval.byte

clean: 
	rm -rf _build *.byte