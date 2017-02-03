
WINTERSMITH=node_modules/wintersmith/bin/wintersmith

.PHONY: build deps

all: build

deps:
	npm install

node_modules: package.json
	npm install

build: node_modules
	$(WINTERSMITH) build

serve: node_modules
	$(WINTERSMITH) preview

DEST = ~/sync/sinusoidal/
copy:
	rsync -av build/* $(DEST)
