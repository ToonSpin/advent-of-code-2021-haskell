SOURCE_FILES = src/day??.hs
BIN_FILES = $(addprefix bin/, $(notdir $(basename $(wildcard $(SOURCE_FILES)))))

all: $(BIN_FILES)

bin/%: src/%.hs
	ghc --make $< -outputdir build -o $@ && rm -r build/*
