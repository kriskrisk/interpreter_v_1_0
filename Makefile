WORKING_DIR = $(shell pwd)
COMP = ghc

all:
	$(COMP) -i$(WORKING_DIR)/bnfc:$(WORKING_DIR)/src -o interpreter src/Main.hs

clean:
	-rm -f $(WORKING_DIR)/src/*.hi $(WORKING_DIR)/src/*.o
