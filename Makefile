# Cyclone-winds - package manager for Cyclone Scheme
# Copyright (c) 2019, Cyclone Team
# All rights reserved.

# Commands
CYCLONE    = cyclone
INSTALL   ?= install
RM        ?= rm -f

# Files
SOURCE = cyclone-winds.scm
BINARY = cyclone-winds

# Path
PREFIX	?= /usr/local
DESTDIR  = $(PREFIX)/bin

TEST = test.scm
TEST_BINARY = test

$(BINARY) : $(SOURCE)
	$(CYCLONE) $< 

# Primary rules (of interest to an end user)
.PHONY: all clean install uninstall full test
all : $(BINARY)

test : all
	$(CYCLONE) $(TEST) && ./$(TEST_BINARY)

clean :
	rm -rf $(BINARY) *.so *.o *.a *.out *.c *.meta $(TEST_BINARY)

install : $(BINARY)
	$(INSTALL) -m0755 $(BINARY) $(DESTDIR)


uninstall :
	$(RM) $(DESTDIR)/$(BINARY)

full : 
	$(MAKE) clean; $(MAKE) && sudo $(MAKE) install && $(MAKE) test
