
.PHONY: build

all: build

build:
	wintersmith build

serve:
	wintersmith preview

DEST = ~/sync/sinusoidal/
copy:
	rsync -av build/* $(DEST)
