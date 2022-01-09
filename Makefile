HS_SRC := $(shell find src -type f) jacinda.cabal

man/ja.1: man/MANPAGE.md
	pandoc $< -s -t man -o $@

docs: doc/guide.pdf doc/guide.html docs/index.html

docs/index.html: doc/guide.html
	cp $^ $@

doc/guide.pdf: doc/guide.md
	pandoc $^ -o $@ --toc

doc/guide.html: doc/guide.md
	pandoc -s $^ -o $@ --toc

install: man/ja.1
	cabal install exe:ja --overwrite-policy=always -w ghc-9.2.1
	strip $$(which ja)
	cp man/ja.1 $(HOME)/.local/share/man/man1

clean:
	rm -rf dist-newstyle moddeps.svg doc/guide.html *.hp *.prof

moddeps.svg: $(HS_SRC)
	graphmod -i src | dot -Tsvg -o $@
