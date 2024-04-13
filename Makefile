.PHONY: build
build:
	@rm -f ./main
	opam exec -- dune build
	@cp _build/install/default/bin/cs3110_compiler ./main
	@chmod u+x ./main

.PHONY: test 
test: build 
	opam exec -- dune test

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

