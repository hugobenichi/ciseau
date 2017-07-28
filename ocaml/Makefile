OUTDIR=build

.DEFAULT_GOAL := build

$(OUTDIR)/ciseau: ciseau.ml
	mkdir -p $(@D)
	ocamlc -o $@ ciseau.ml

build: $(OUTDIR)/ciseau

run: build
	$(OUTDIR)/ciseau

clean:
	rm *cmi
	rm *cmo
	rm -rf $(OUTDIR)
