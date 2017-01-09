include ../config

EXAMPLE = qpa.zls

all: byte

byte: $(EXAMPLE:%.zls=%.byte)

parser.ml: parser.cmi symulib.cmi

qpa.byte: symb.cmo parser.cmo lexer.cmo bndrel.cmo clk.cmo draw.cmo dbm.cmo symulib.cmo qpa.cmo qpa_main.cmo 
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(INCLUDES) \
	    -I $(ZLLIB) $(ZLSTDLIBS) $(ZLEXTRALIBS) graphics.cma $^

qpa_main.cmo: INCLUDE += $(SUNDIALS)


qpa.ml qpa_main.ml: symulib.zci qpa.zls 
	$(ZELUC) -I $(ZLLIB) $(ZLCFLAGS) -s main -sampling 0 qpa.zls
	mv main.ml qpa_main.ml


depend: .depend
.depend:
	$(OCAMLDEP) $(INCLUDE) *.mli *.ml > .depend


clean:
	-@rm -f $(EXAMPLE:.zls=.ml) $(EXAMPLE:.zls=_main.ml)
	-@rm -f parser.ml parser.mli lexer.ml
	-@rm -f *.o *.cm[oix] *.annot *.obc *.zci .depend
	-@rm -f $(EXAMPLE:%.zls=%.byte)

realclean cleanall: clean

cleanall: realclean

