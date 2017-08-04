OUTDIR=build

.DEFAULT_GOAL := build

builddir:
	mkdir -p $(OUTDIR)

$(OUTDIR)/ciseau: ciseau.ml
	ocamlc unix.cma -o $@ ciseau.ml

$(OUTDIR)/teststty: stty.ml
	ocamlc unix.cma -o $@ stty.ml

build: builddir $(OUTDIR)/ciseau $(OUTDIR)/teststty

color: build
	$(OUTDIR)/ciseau

rawmode: build
	$(OUTDIR)/teststty

clean:
	rm *cmi
	rm *cmo
	rm -rf $(OUTDIR)
