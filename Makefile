# Cyclone-winds - package manager for Cyclone Scheme
# Copyright (c) 2019, Arthur Maciel
# All rights reserved.

CW_PLATFORM := $(shell uname -s)

# Commands
CYCLONE    = cyclone #-CP -DCW_PLATFORM=\"$(CW_PLATFORM)\"
INSTALL   ?= install
RM        ?= rm -f

# Files
SOURCE = cyclone-winds.scm
BINARY = cyclone-winds

# Path
PREFIX	?= /usr/local
DESTDIR  = $(PREFIX)/bin

# TESTS = $(basename $(TEST_SRC))

# Primary rules (of interest to an end user)
all : $(BINARY)

$(BINARY) : $(SOURCE)
	$(CYCLONE) $< 

# test : libs $(TESTS)

clean :
	rm -rf $(BINARY) *.so *.o *.a *.out *.c *.meta

install : $(BINARY)
	$(INSTALL) -m0755 $(BINARY) $(DESTDIR)


uninstall :
	$(RM) $(DESTDIR)$(BINARY)


.PHONY: all clean install uninstall full

full : 
	$(MAKE) clean; $(MAKE) && sudo $(MAKE) install
