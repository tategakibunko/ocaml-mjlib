include ../../Makefile.def
PKGS:=extlib oUnit
SRCS:=\
	test_utils.ml\
	test_pai.ml\
	test_agari.ml\
	test_machi.ml\
	test_naki.ml\
	test_mentsu.ml\
	test_parser.ml\
	test_fetch.ml\
	test_fu.ml\
	test_jicha.ml\
	test_yama.ml\
	test_point.ml\
	test.ml\

all: test

test: $(SRCS)
	$(OPT) -thread -o $@ -linkpkg -package "$(PKGS)" -I ../ mjlib.cmxa $(SRCS)

rebuild:
	make clean
	make all

clean:
	rm -f *.cm* *.o test

