.PHONY: build
build:
	@rm -f ./main
	dune build
	@cp _build/install/default/bin/x86ISTMB ./main
	@chmod u+x ./main

.PHONY: test 
test: build 
	dune test

.PHONY: clean
clean:
	dune clean
	@rm -rf ./main
