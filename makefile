
LIB=bintree.cmo common.cmo myseq.cmo
CORE=focus.cmo syntax.cmo lis.cmo
WEBAPP=jsutils.cmo webapp.cmo html.cmo widget_focus.cmo widget_suggestions.cmo widget_table.cmo widget_commandline.cmo
JSONIQ=jsoniq.cmo jsoniq_focus.cmo jsoniq_semantics.cmo jsoniq_syntax.cmo jsoniq_command.cmo jsoniq_files.cmo rdf.cmo jsoniq_functions.cmo jsoniq_suggestions.cmo jsoniq_lis.cmo

FLAGS= -package num,str,unix,yojson,ppx_deriving_yojson,csv,js_of_ocaml,js_of_ocaml-ppx -I ../../../lib -I ../../core -I ../../core/webapp

all: $(JSONIQ) jsoniq_webapp.ml
	ocamlfind ocamlc $(FLAGS) -package js_of_ocaml,js_of_ocaml-lwt -linkpkg -o html/script.byte $(LIB) $(CORE) $(WEBAPP) $(JSONIQ) jsoniq_webapp.ml
	js_of_ocaml html/script.byte

install:
	cp -r html/* /local/ferre/web/ferre/dexteris

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -c $(FLAGS) $<
