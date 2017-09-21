OUTDIR=build

.DEFAULT_GOAL := build

builddir:
	mkdir -p $(OUTDIR)

$(OUTDIR)/ciseau: ciseau.ml $(OUTDIR)/ioctl.o
	ocamlopt $(OUTDIR)/ioctl.o unix.cmxa -o $@ $<
#	ocamlc -custom $(OUTDIR)/ioctl.o unix.cma -o $@ $<

$(OUTDIR)/ioctl.o: ioctl.c
	gcc -c -o $@ $< -I /usr/local/lib/ocaml

build: builddir $(OUTDIR)/ciseau $(OUTDIR)/ioctl.o

run: build
	$(OUTDIR)/ciseau

clean:
	rm -f *\.cmi
	rm -f *\.cmo
	rm -f *\.cmx
	rm -f *\.o
	rm -rf $(OUTDIR)
