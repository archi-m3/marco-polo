emacs ?= emacs

.PHONY: clean
clean:
	if [ -d build ]; then rm -r build; fi

.PHONY: build
build: clean
	@mkdir -p build
	@cat *.el > build/marco-polo.el

.PHONY: run
run: build
	$(emacs) -q -l build/marco-polo.el
