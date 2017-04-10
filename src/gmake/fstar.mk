ifndef FSTAR_HOME
   $(error "Please define the `FSTAR_HOME` variable before including this makefile.")
endif

HINTS_ENABLED?=--use_hints

FSTAR_ALWAYS=$(shell cd $(FSTAR_HOME) && pwd)/bin/fstar.exe $(OTHERFLAGS) $(HINTS_ENABLED)
FSTAR=$(FSTAR_ALWAYS)


CVEREXE_ALWAYS=$(shell cd $(FSTAR_HOME) && pwd)/bin/fabc-make.exe
CVEREXE=$(CVEREXE_ALWAYS)

DG=$(.DEFAULT_GOAL)
$(FSTAR_HOME)/batch-ids.tmp:
	$(CVEREXE) create > $(FSTAR_HOME)/batch-ids.tmp
.DEFAULT_GOAL := $(DG)

CVERCONFIG=$(FSTAR_HOME)/batch-ids.tmp
CVERDIR=$(subst $(abspath $(FSTAR_HOME))/,,$(abspath $(shell pwd)))
CVERFSTAR=$(CVEREXE) add -i $(CVERCONFIG) -d 'CURRENT_DIR' -- \$$H/bin/fstar.exe $(OTHERFLAGS) $(HINTS_ENABLED)
