BUILD_DIR = build
SOURCE_FILES = src/day??.hs
BIN_FILES = $(addprefix bin/, $(notdir $(basename $(wildcard $(SOURCE_FILES)))))

all: $(BIN_FILES)

$(BIN_FILES): $(SOURCE_FILES)
	ghc --make $< -outputdir build -o $@
