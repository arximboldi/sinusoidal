
NIX_SHELL = nix-shell

ifeq ($(NIX_STORE),)
    eval_ = $(NIX_SHELL) ./shell.nix --pure --run
else
    eval_ = eval
endif

all: _site
.PHONY: clean

site: site.hs
	$(eval_) "ghc --make site.hs -outputdir tmp"

_site: site
	$(eval_) "./site build"

clean: site
	$(eval_) "./site clean"
