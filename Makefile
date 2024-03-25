.PHONY: build
build:
	@rm -f ./main
	dune build
	@cp _build/install/default/bin/cs3110_compiler ./main
	@chmod u+x ./main

.PHONY: clean
clean:
	dune clean 
	@rm -rf ./main
