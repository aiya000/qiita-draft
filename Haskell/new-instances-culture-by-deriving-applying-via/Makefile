all: build

build:
	markdown-pp content.mdpp -o content.md

watch:
	watchexec --exts mdpp,hs -- $(MAKE) build

open:
	grip --pass "$(github-token)" --browser content.md &

start:
	$(MAKE) open watch
