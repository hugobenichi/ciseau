# TODO: check that ocaml is on $PATH or run `eval 'opam config env'`

.DEFAULT_GOAL := build

OUTDIR=build

builddir:
	mkdir -p $(OUTDIR)

$(OUTDIR)/ciseau: ciseau.ml $(OUTDIR)/ioctl.o
	ocamlopt -p -g -inline 0 $(OUTDIR)/ioctl.o str.cmxa unix.cmxa -o $@ $<
#	ocamlc -custom $(OUTDIR)/ioctl.o unix.cma -o $@ $<

$(OUTDIR)/ioctl.o: ioctl.c
	gcc -v -c -o $@ $< -I `ocamlc -where`

build: builddir $(OUTDIR)/ciseau $(OUTDIR)/ioctl.o

run: build
	$(OUTDIR)/ciseau

clean:
	rm -f *\.cmi
	rm -f *\.cmo
	rm -f *\.cmx
	rm -f *\.o
	rm -rf $(OUTDIR)
