PY	:= $(shell which python3 || which python || which pypy3 || which pypy)

.PHONY: build
build:
	@rm -f ./main
	opam exec -- dune build
	@cp _build/install/default/bin/x86ISTMB ./main
	@chmod u+x ./main
	@chmod u+x .githooks/pre-commit
	@make README

.PHONY: protect
protect:
	sudo chmod +x .githooks/pre-commit

.PHONY: README
README:
	$(PY) readme.py

.PHONY: test 
test: build 
	opam exec -- dune exec ./test/test_x86ISTMB.exe

.PHONY: quick_test
quick_test: build
	opam exec -- dune exec ./test/test_x86ISTMB.exe -- --quick-tests

.PHONY: utop
utop: README
	dune utop

.PHONY: bisect
bisect: README
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
# 	echo "open X86ISTMB;;" | dune utop

.PHONY: clean
clean: README
	opam exec -- dune clean
	@rm -rf ./main

.PHONY: docs
docs: README
	@opam exec -- dune build @doc
	@echo '==> Docs written to _build/default/_doc/_html/'

.PHONY: deps
deps: 
	opam install . --deps-only --with-test --with-doc

PORT	:= 8003
.PHONY: serve 
serve: docs
	@echo '==> Serving at localhost:$(PORT)'
	@cd _build/default/_doc/_html/; $(shell which python || which python3) -m http.server $(PORT)

.PHONY: cloc 
cloc: 
	@make build > /dev/null
	@echo "$$(cloc --by-file --include-lang=OCaml bin lib test --json | jq .SUM.code) lines of code"
