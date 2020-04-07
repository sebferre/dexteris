
LIB=bintree.cmo myseq.cmo
CORE=focus.cmo syntax.cmo lis.cmo
WEBAPP=jsutils.cmo webapp.cmo html.cmo widget_focus.cmo widget_suggestions.cmo widget_table.cmo
JSONIQ=jsoniq.cmo jsoniq_json.cmo jsoniq_focus.cmo jsoniq_semantics.cmo jsoniq_syntax.cmo jsoniq_suggestions.cmo jsoniq_lis.cmo

FLAGS= -package yojson,lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml-ppx -I ../../../lib -I ../../core -I ../../core/webapp

all: $(JSONIQ) jsoniq_webapp.ml
	ocamlfind ocamlc $(FLAGS) -linkpkg -o html/script.byte $(LIB) $(CORE) $(WEBAPP) $(JSONIQ) jsoniq_webapp.ml
	js_of_ocaml html/script.byte

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -c $(FLAGS) $<
