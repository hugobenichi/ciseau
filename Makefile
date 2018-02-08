# TODO: check that ocaml is on $PATH or run `eval 'opam config env'`

OUTDIR=build

CAML_LD_LIBRARY_PATH?=/usr/local/lib/ocaml/
CAML=$(CAML_LD_LIBRARY_PATH)/../ocaml

.DEFAULT_GOAL := build

builddir:
	mkdir -p $(OUTDIR)

$(OUTDIR)/ciseau: ciseau.ml $(OUTDIR)/ioctl.o
	ocamlopt -p -g -inline 0 $(OUTDIR)/ioctl.o unix.cmxa -o $@ $<
#	ocamlc -custom $(OUTDIR)/ioctl.o unix.cma -o $@ $<

$(OUTDIR)/ioctl.o: ioctl.c
	gcc -c -o $@ $< -I $(CAML)

build: builddir $(OUTDIR)/ciseau $(OUTDIR)/ioctl.o

run: build
	$(OUTDIR)/ciseau

clean:
	rm -f *\.cmi
	rm -f *\.cmo
	rm -f *\.cmx
	rm -f *\.o
	rm -rf $(OUTDIR)
