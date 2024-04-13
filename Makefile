.PHONY: build
build:
	@rm -f ./main
	opam exec -- dune build
	@cp _build/install/default/bin/x86ISTMB ./main
	@chmod u+x ./main

.PHONY: test 
test: build 
	opam exec -- dune test

.PHONY: clean
clean:
	opam exec -- dune clean
	@rm -rf ./main
