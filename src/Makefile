OPT:=ocamlfind ocamlopt -g
CALC:=ocamlfind ocamlc -g
LIBNAME:=mjlib
PACKNAME:=Mjlib
PKGS:=extlib
VERSION:=1.0.0

MLS:=\
	utils.ml\
	types.ml\
	pai.ml\
	kawahai.ml\
	kawa_action.ml\
	fetch.ml\
	relative_pos.ml\
	jicha.ml\
	bafu.ml\
	mentsu.ml\
	machi.ml\
	naki.ml\
	yaku.ml\
	fu.ml\
	wanpai.ml\
	tumohai.ml\
	yama.ml\
	agari.ml\
	parser.ml\
	point.ml\
	payment.ml\
	ryukyoku.ml\
	chonbo.ml\
	tehai.ml\
	player.ml\
	game.ml\

#MLIS:=$(MLS:.ml=.mli)
CMXS:=$(MLS:.ml=.cmx)
#CMIS:=$(MLIS:.mli=.cmi)

all:$(LIBNAME).cmxa META

$(LIBNAME).cmx:$(CMIS) $(CMXS)
	$(OPT) -pack -o $@ $(CMXS)

$(LIBNAME).cmxa:$(LIBNAME).cmx
	$(OPT) -a -o $@ $<

%.mli:%.ml
	$(CAMLC) -package "$(PKGS)" -i $< > $@

%.cmi:%.mli
	$(CAMLC) -package "$(PKGS)" -for-pack $(PACKNAME) -c $<

%.cmx:%.ml
	$(OPT) -package "$(PKGS)" -for-pack $(PACKNAME) -c $<

META:Makefile
	echo "description = \"ocaml mahjong library\"" > $@
	echo "version = \"$(VERSION)\"" >> $@
	echo "archive(byte) = \"$(LIBNAME).cma\"" >> $@
	echo "archive(native) = \"$(LIBNAME).cmxa\"" >> $@
	echo "requires = \"$(PKGS)\"" >> $@

install:
	ocamlfind install $(LIBNAME) META $(LIBNAME).*
#	ocamlfind install $(LIBNAME) META $(LIBNAME).* *.mli

uninstall:
	ocamlfind remove $(LIBNAME)

reinstall:
	make uninstall
	make install

clean:
	rm -f *.cmi *.cmo *.cmx *.o *.a *.out META

rebuild:
	make clean
	make all

