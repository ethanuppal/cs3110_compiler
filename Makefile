PY	:= $(shell which python3 || which python || which pypy3 || which pypy)

.PHONY: build
build:
	@rm -f ./main
	opam exec -- dune build
	@cp _build/install/default/bin/cs3110_compiler ./main
	@chmod u+x ./main
	@chmod u+x .githooks/pre-commit
	@make README

.PHONY: README
README:
	$(PY) readme.py

.PHONY: test 
test: build 
	opam exec -- dune test

.PHONY: bisect
bisect:
	@find . -name '*.coverage' | xargs rm -f
	@OUNIT_CI=true dune test --instrument-with bisect_ppx --force
	@bisect-ppx-report html
	@if [ $$(command -v pup) ]; then \
		printf "Overall Coverage: "; \
		cat _coverage/index.html | pup html body div#header h2 text{}; \
	fi

.PHONY: view
view:
	open _coverage/index.html

# .PHONY: utop 
# utop: 
# 	echo "open Cs3110_compiler;;" | dune utop

.PHONY: clean
clean:
	opam exec -- dune clean
	@rm -rf ./main

.PHONY: docs
docs:
	@ocamldoc -html -d docs/html lib/*.mli lib/*.ml 2>/dev/null || echo "==> (Error output surpressed for ocamldoc)"
	@echo '==> Docs written to docs/html'

PORT	:= 8003
.PHONY: serve 
serve: docs
	@echo '==> Serving at localhost:$(PORT)'
	@cd docs/html; $(shell which python || which python3) -m http.server $(PORT)

.PHONY: cloc 
cloc: 
	@make build > /dev/null
	@echo "$$(cloc bin lib --json | jq .SUM.code) lines of code"
