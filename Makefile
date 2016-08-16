
default: elm.js

ELM_FILES = $(shell find . -path ./elm-stuff -prune -o -path ./lib -prune -o -type f -name "*.elm" | grep "\.elm" | grep -v "examples")

# ELM_PATH := /usr/local/bin/
# PATH := $(ELM_PATH)/bin:$(PATH)

elm.js: $(ELM_FILES)
	elm-make --yes $(ELM_FILES) --output ./elm.js

clean-deps:
	rm -rf elm-stuff

clean:
	rm -f *.js
	rm -rf elm-stuff/build-artifacts
