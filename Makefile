SOURCES=$(shell find src/Twimg -type f)
PULP=npx pulp
PSC=npx psc-package
PPSC=$(PULP) --psc-package
WEBPACK=npx webpack

.PHONY: default
default: asset_pipeline serve_bg watch

watch:
	$(PPSC) --watch build --to ./dist/Main.js
asset_pipeline:
	cp -r html/* dist

.PHONY: build
build:
	$(PPSC) build --to ./dist/Main.js

dist/Main.js:
	$(PPSC) build --to ./dist/Main.js

serve:
	python -m SimpleHTTPServer 8080

serve_bg:
	python -m SimpleHTTPServer 8080 &

clean:
	rm -rf dist
.PHONY: test
test:
	$(PPSC) --watch test
