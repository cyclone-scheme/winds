# Winds - package manager for Cyclone Scheme
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
SYS_DIR    = sys

# Files
WINDS_SRC  = winds.scm
LIBS_SRC   = $(wildcard $(LIBS_DIR)/*.sld)
TESTS_SRC  = $(wildcard $(TESTS_DIR)/*.scm)
SYS_SRC  = $(SYS_DIR)/tasks.scm

# Output 
WINDS_BN   = $(basename $(WINDS_SRC))
LIBS_BN    = $(LIBS_SRC:.sld=.so)
TESTS_BN   = $(basename $(TESTS_SRC))
SYS_BN   = $(basename $(SYS_SRC))

#Rules
all : $(WINDS_BN)

$(WINDS_BN) : $(LIBS_BN) $(WINDS_SRC)
	$(CYCLONE) $(WINDS_SRC)

$(LIBS_BN) : %.so : %.sld
	$(CYCLONE) $<

$(TESTS_BN) : % : %.scm
	$(CYCLONE) $<

$(SYS_BN) : 
	$(CYCLONE) $(SYS_SRC)

.PHONY: run-tests test clean install uninstall full
run-tests : $(TESTS_BN)
	./$<

test : all run-tests

run-sys-tasks : $(SYS_BN)
	./$<

sys-tasks: all run-sys-tasks

clean :
	$(RM) $(WINDS_BN) *.a *.out *.so *.o *.c *.meta tags 
	$(RM) $(LIBS_DIR)/*.so $(LIBS_DIR)/*.o $(LIBS_DIR)/*.c $(LIBS_DIR)/*.meta 
	$(RM) $(TESTS_DIR)/*.so $(TESTS_DIR)/*.o $(TESTS_DIR)/*.c $(TESTS_DIR)/*.meta $(TESTS_BN) 
	$(RM) $(SYS_DIR)/*.so $(SYS_DIR)/*.o $(SYS_DIR)/*.c $(SYS_DIR)/*.meta $(SYS_BN)

install : $(WINDS_BN)
	$(INSTALL) -m0755 $(WINDS_BN) $(DEST_DIR)

uninstall :
	$(RM) $(DEST_DIR)/$(WINDS_BN)

full : 
	$(MAKE) clean; $(MAKE) && sudo $(MAKE) install && $(MAKE) test
