# Cyclone-winds - package manager for Cyclone Scheme
# Copyright (c) 2019, Arthur Maciel
# All rights reserved.

#include Makefile.config

# Commands
CYCLONE = cyclone -A .
INSTALL   ?= install
RM        ?= rm -f

# Files
SOURCE = cyclone-winds.scm
BINARY = cyclone-winds

# Path
DESTDIR = "/usr/local/bin/"

# TESTS = $(basename $(TEST_SRC))

# Primary rules (of interest to an end user)
all : $(BINARY)

$(BINARY) : $(SOURCE)
	$(CYCLONE) $< 

# test : libs $(TESTS)

clean :
	rm -rf $(BINARY) *.so *.o *.a *.out *.c *.meta

install : 
	$(INSTALL) -m0755 $(BINARY) $(DESTDIR)


uninstall :
	$(RM) $(DESTDIR)$(BINARY)


.PHONY: all clean install uninstall full

full : 
	make clean; make && sudo make install
