include ../config

EXAMPLE=clock.zls

all: byte
#$(targets)

byte: $(EXAMPLE:%.zls=%.byte)
opt:  $(EXAMPLE:%.zls=%.opt)

clock.byte: draw.cmo clock.cmo clock_main.cmo
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(INCLUDES) \
	    -I $(ZLLIB) $(ZLSTDLIBS) $(ZLEXTRALIBS) graphics.cma $^

clock.opt: draw.cmx clock.cmx clock_main.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $(INCLUDES) \
	    -I $(ZLLIB) $(ZLSTDLIBS:.cma=.cmxa) $(ZLEXTRALIBS:.cma=.cmxa) graphics.cmxa $^

clock_main.cmo: INCLUDE += $(SUNDIALS)
clock_main.cmx: INCLUDE += $(SUNDIALS)

clock.ml clock_main.ml: draw.zci clock.zls
	$(ZELUC) -I $(ZLLIB) $(ZLCFLAGS) -s main -sampling 0 clock.zls
	mv main.ml clock_main.ml

depend: .depend
.depend:
	$(OCAMLDEP) $(INCLUDE) *.mli *.ml > .depend

clean:
	-@rm -f $(EXAMPLE:.zls=.ml) $(EXAMPLE:.zls=_main.ml)
	-@rm -f *.o *.cm[oix] *.annot *.obc *.zci .depend
	-@rm -f $(EXAMPLE:%.zls=%.byte) $(EXAMPLE:%.zls=%.opt)

realclean cleanall: clean

-include .depend

