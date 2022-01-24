.PHONY: all
all: test

.PHONY: test
test:
	stack test --fast

.PHONY: build
build:
	stack build --fast
