OUTDIR=build

.DEFAULT_GOAL := build

builddir:
	mkdir -p $(OUTDIR)

$(OUTDIR)/ciseau: ciseau.ml
	ocamlc unix.cma -o $@ ciseau.ml

$(OUTDIR)/teststty: stty.ml
	ocamlc unix.cma -o $@ stty.ml

$(OUTDIR)/ioctl.o: ioctl.c
	gcc -o $@ -c ioctl.c -I /usr/local/lib/ocaml

build: builddir $(OUTDIR)/ciseau $(OUTDIR)/teststty $(OUTDIR)/ioctl.o

run: build
	$(OUTDIR)/ciseau

teststty: build
	$(OUTDIR)/teststty

clean:
	rm *cmi
	rm *cmo
	rm -rf $(OUTDIR)
