emacs ?= emacs

.PHONY: all
all: build

.PHONY: run
run: build
	$(emacs) -q -l build/marco-polo.el
	make clean

.PHONY: build
build:
	@mkdir -p build
	@cat *.el > build/marco-polo.el

.PHONY: clean
clean:
	rm -r build
