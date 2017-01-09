include ../config

EXAMPLE = fgs.zls

all: byte

byte: $(EXAMPLE:%.zls=%.byte)

fgs.byte: misc.cmo fgs.cmo fgs_main.cmo
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(INCLUDES) \
	    -I $(ZLLIB) $(ZLSTDLIBS) $(ZLEXTRALIBS) graphics.cma $^

fgs_main.cmo: INCLUDE += $(SUNDIALS)

fgs.ml fgs_main.ml: misc.zci fgs.zls
	$(ZELUC) -I $(ZLLIB) $(ZLCFLAGS) -i -s main -sampling 0 fgs.zls
	mv main.ml fgs_main.ml

misc.zci:
	$(ZELUC) -I $(ZLLIB) $(ZLCFLAGS) -i misc.zli

depend: .depend
.depend:
	$(OCAMLDEP) $(INCLUDE) *.mli *.ml > .depend

clean:
	-@rm -f $(EXAMPLE:.zls=.ml) $(EXAMPLE:.zls=_main.ml)
	-@rm -f *.o *.cm[oix] *.annot *.obc *.zci .depend
	-@rm -f $(EXAMPLE:%.zls=%.byte)

realclean cleanall: clean

-include .depend

