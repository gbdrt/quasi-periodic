include ../config

EXAMPLE = bp_ltta.zls tb_ltta.zls rb_ltta.zls gc_ltta.zls

all: byte

byte: $(EXAMPLE:%.zls=%.byte)


qpa.zls: misc.zci
qpa.ml qpa.zci: qpa.zls
	$(ZELUC) -I $(ZLLIB) $(ZELUCFLAGS) $<

misc.zci:
	$(ZELUC) -I $(ZLLIB) $(ZLCFLAGS) misc.zli

%_ltta.byte: misc.cmo qpa.cmo %_ltta.cmo %_ltta_main.cmo 
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(INCLUDES) \
	    -I $(ZLLIB) $(ZLSTDLIBS) $(ZLEXTRALIBS) graphics.cma $^

%_ltta_main.cmo: INCLUDE += $(SUNDIALS)

%_ltta.ml %_ltta_main.ml: misc.zci qpa.zci %_ltta.zls 
	$(ZELUC) -I $(ZLLIB) $(ZLCFLAGS) -s main -sampling 0 $*_ltta.zls
	mv main.ml $*_ltta_main.ml

depend: .depend
.depend:
	$(OCAMLDEP) $(INCLUDE) *.mli *.ml > .depend


clean:
	-@rm -f $(EXAMPLE:.zls=.ml) $(EXAMPLE:.zls=_main.ml)
	-@rm -f *.o *.cm[oix] *.annot *.obc *.zci .depend
	-@rm -f $(EXAMPLE:%.zls=%.byte)

realclean cleanall: clean

-include .depend

