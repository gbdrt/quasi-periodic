include ../config

all:
	$(ZELUC) -I $(ZLLIB) -i buffer.zli
	$(ZELUC) -I $(ZLLIB) -i qsa.zls

clean:
	-@rm -f qsa.ml *.zci

realclean cleanall: clean

