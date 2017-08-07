
NIX_SHELL = nix-shell

ifeq ($(NIX_STORE),)
    eval_ = $(NIX_SHELL) ./shell.nix --pure --run
else
    eval_ = eval
endif

all: _site
.PHONY: clean _site

gen: gen.hs
	$(eval_) "ghc --make gen.hs -outputdir tmp"

_site: gen
	$(eval_) "./gen build"

clean: gen
	$(eval_) "./gen clean"

server: gen
	$(eval_) "./gen server"

watch: gen
	$(eval_) "./gen watch"

DEST = ~/sync/sinusoidal/
copy: _site
	rsync -av _site/* $(DEST)
