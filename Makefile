.PHONY: info init all test clean fmt

info:
	echo "make [init|all|test|clean|fmt]"

init:
	eval `(opam env)`

all:
	dune build

test:
	dune runtest -j 1

doc:
	dune build @doc

clean:
	dune clean

fmt:
	dune build @fmt --auto-promote