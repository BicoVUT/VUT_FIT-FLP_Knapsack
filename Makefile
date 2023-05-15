# Compiler options
GHC = ghc
GHC_FLAGS = -Wall

# Source files
SRC = src/Main.hs src/BruteForce.hs src/ParseArg.hs	src/GeneticAlgo.hs

# Destination file
DEST = flp22-fun

all: $(DEST)

$(DEST): $(SRC)
	$(GHC) $(GHC_FLAGS) --make -o $(DEST) $(SRC)

clean:
	rm -f $(DEST) *.o *.hi