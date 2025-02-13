.PHONY: clean install release darwin-release fmt fix check

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules -j

HC ?= ghc

HS_SRC := $(shell find src -type f) jacinda.cabal

JAC_SRC := $(shell find prelude lib -type f)

VERSION := $(shell grep -o '[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*' jacinda.cabal | head -n1)

GR_OPTIONS := -u vmchale -r jacinda -t $(VERSION)

man/ja.1: man/MANPAGE.md
	pandoc $< -s -t man -o $@

docs: doc/guide.pdf doc/guide.html docs/index.html

BINS := bin/x86_64-linux-ja \
    bin/arm-linux-gnueabihf-ja \
    bin/aarch64-linux-ja \
    bin/powerpc64le-linux-ja

bins: $(BINS)

docs/index.html: doc/guide.html
	cp $^ $@

doc/guide.pdf: doc/guide.md
	pandoc $^ -o $@ --toc --pdf-engine=lualatex -V 'monofont:JetBrains Mono'

doc/guide.html: doc/guide.md
	pandoc -s $^ -o $@ --toc

install: $(HS_SRC) man/ja.1
	cabal install -w $(HC)
	cp man/ja.1 $(HOME)/.local/share/man/man1

clean:
	make -C vscode clean
	make -C tex clean
	rm -rf tags tags.mtime dist-newstyle moddeps.svg doc/guide.html *.hp *.prof bench/data/*.txt bin

moddeps.svg: $(HS_SRC)
	graphmod -i src | dot -Tsvg -o $@

release: $(BINS)
	github-release release $(GR_OPTIONS)
	for bin in $(notdir $^) ; do \
	    github-release upload $(GR_OPTIONS) -n $$bin -f bin/$$bin --replace ; \
	done
	github-release upload $(GR_OPTIONS) -n ja.1 -f man/ja.1 --replace

darwin-release: bin/aarch64-darwin-ja
	github-release upload $(GR_OPTIONS) -n aarch64-darwin-ja -f bin/aarch64-darwin-ja --replace
	github-release upload $(GR_OPTIONS) -n aarch64-librure.dylib -f /usr/local/lib/librure.dylib --replace

bin/aarch64-darwin-ja: $(HS_SRC)
	mkdir -p $(dir $@)
	cabal build exe:ja -w $(HC)
	export BIN=$$(cabal-plan list-bins | awk '/ja$$/ {print $$2}'); \
	    cp $$BIN $@ ; \
	    strip $@

bin/x86_64-linux-ja: $(HS_SRC)
	@mkdir -p $(dir $@)
	cabal build exe:ja -w $(HC) --builddir=dist-newstyle/x86-linux --enable-executable-static
	export BIN=$$(fd 'x86_64-linux.*ja$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    strip $@

bin/arm-linux-gnueabihf-ja: $(HS_SRC)
	@mkdir -p $(dir $@)
	@cabal build --with-ghc arm-linux-gnueabihf-ghc-9.2 --with-ghc-pkg arm-linux-gnueabihf-ghc-pkg-9.2 --project-file cabal.project.cross exe:ja --enable-executable-static --builddir=dist-newstyle/arm-linux
	export BIN=$$(fd 'arm-linux.*ja$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    arm-linux-gnueabihf-strip $@

bin/aarch64-linux-ja: $(HS_SRC)
	@mkdir -p $(dir $@)
	@cabal build --with-ghc aarch64-linux-gnu-ghc-9.2 --with-ghc-pkg aarch64-linux-gnu-ghc-pkg-9.2 --project-file cabal.project.cross exe:ja --enable-executable-static --builddir=dist-newstyle/aarch64-linux
	export BIN=$$(fd 'aarch64-linux.*ja$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    aarch64-linux-gnu-strip $@

bin/mips64-linux-ja: $(HS_SRC)
	@mkdir -p $(dir $@)
	@cabal build --with-ghc mips64-linux-gnuabi64-ghc-9.2 --with-ghc-pkg mips64-linux-gnuabi64-ghc-pkg-9.2.2 --project-file cabal.project.cross exe:ja --enable-executable-static --builddir=dist-newstyle/mips-linux
	export BIN=$$(fd 'mips-linux.*ja$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    mips64-linux-gnuabi64-strip $@

bin/powerpc64le-linux-ja: $(HS_SRC)
	@mkdir -p $(dir $@)
	@cabal build --with-ghc powerpc64le-linux-gnu-ghc-9.2 --with-ghc-pkg powerpc64le-linux-gnu-ghc-pkg-9.2 --project-file cabal.project.cross exe:ja --enable-executable-static --builddir=dist-newstyle/powerpc64le-linux
	export BIN=$$(fd 'powerpc64le-linux.*ja$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    powerpc64le-linux-gnu-strip $@

tags: $(JAC_SRC) $(HS_SRC)
	rm -f tags
	ghc-tags --ctags
	fd '.jac$$' prelude lib -x ja run examples/tags.jac -i >> $@
	ctags --append=yes --languages=ALEX,HAPPY -R src

bench: bench/data/lines.txt bench/data/span.txt bench/data/ulysses.txt

bench/data/lines.txt: test/examples/data/1.txt
	perl -0777pe '$$_=$$_ x 10' $^ > $@

bench/data/span.txt: examples/span.txt
	perl -0777pe '$$_=$$_ x 10000' $^ > $@

bench/data/ulysses.txt:
	curl https://www.gutenberg.org/files/4300/4300-0.txt -o $@

check:
	cabal build -w $(HC)
	fd .jac examples/ -x cabal run ja -w $(HC) -- tc

fmt:
	fd '^[A-Z][[:alpha:]]*\.hs$$' $$(ja -F'\s*:\s*' '{%/hs-source-dirs/}{`2}' -i jacinda.cabal) -x stylish-haskell -i

fix:
	fd '^[A-Z][[:alpha:]]*\.hs$$' $$(ja -F'\s*:\s*' '{%/hs-source-dirs/}{`2}' -i jacinda.cabal) -x ja "{%/infix(r|l)?\s+\d+/}{sprintf '- fixity: %s' \`0}}" -i | ja '~.$$0'
