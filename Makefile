.PHONY: info init all test clean fmt

info:
	echo "make [init|build|test|clean|fmt]"

init:
	eval `(opam env)`

build:
	dune build

test:
	dune runtest -j 1

doc:
	dune build @doc

clean:
	dune clean

fmt:
	dune build @fmt --auto-promote