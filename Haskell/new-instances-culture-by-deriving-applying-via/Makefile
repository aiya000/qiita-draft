all: build

build:
	markdown-pp template.md -o content.md

watch:
	watchexec --exts mdpp,hs -- '$(MAKE) build && echo success || echo failure'

open:
	grip --pass "$(github-token)" --browser content.md &

close:
	killall grip

start:
	$(MAKE) open watch
