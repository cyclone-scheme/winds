# Cyclone-winds - package manager for Cyclone Scheme
# Copyright (c) 2020, Cyclone Team
# All rights reserved.

# Commands
CYCLONE    = cyclone
INSTALL   ?= install
RM        ?= rm -f

# Directories
PREFIX	  ?= /usr/local
DEST_DIR   = $(PREFIX)/bin
LIBS_DIR   = libs
TESTS_DIR  = tests

# Files
WINDS_SRC  = cyclone-winds.scm
LIBS_SRC   = $(wildcard $(LIBS_DIR)/*.sld)
TESTS_SRC  = $(wildcard $(TESTS_DIR)/*.scm)

# Binaries 
WINDS_BN   = $(basename $(WINDS_SRC))
TESTS_BN   = $(basename $(TESTS_SRC))

#Rules
$(WINDS_BN) : $(WINDS_SRC)
	$(CYCLONE) $<

$(TESTS_BN) : $(TESTS_SRC)
	$(CYCLONE) $<
	./$@

.PHONY: all libs clean install uninstall test full
libs : $(LIBS_SRC)
	$(CYCLONE) $<

all : libs $(WINDS_BN)

clean :
	$(RM) $(WINDS_BN) *.a *.out *.so *.o *.c *.meta tags $(LIBS_DIR)/*.so $(LIBS_DIR)/*.o $(LIBS_DIR)/*.c $(LIBS_DIR)/*.meta $(TESTS_DIR)/*.so $(TESTS_DIR)/*.o $(TESTS_DIR)/*.c $(TESTS_DIR)/*.meta $(TESTS_BN)

install : $(WINDS_BN)
	$(INSTALL) -m0755 $(WINDS_BN) $(DEST_DIR)

uninstall :
	$(RM) $(DEST_DIR)/$(WINDS_BN)

test : $(TESTS_BN)
	./$<
full : 
	$(MAKE) clean; $(MAKE) && sudo $(MAKE) install && $(MAKE) test
