.DEFAULT_GOAL := build
OUTDIR=build

builddir:
	mkdir -p $(OUTDIR)

$(OUTDIR)/ciseau: $(OUTDIR)/ioctl.o util.mli util.ml ciseau.ml
	ocamlopt -p -g -inline 0 str.cmxa unix.cmxa -o $@ $^

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
