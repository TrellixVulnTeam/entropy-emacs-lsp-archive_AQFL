# SHELL := /bin/bash
DOIT=emacs -q --batch -l ./eemacs-lsp-archive-load.el

define DoExtra =
	bash -c	"if [[ -f elements/library/eemacs-lspa-install.sh ]];then cd elements/library && bash eemacs-lspa-install.sh;fi"
endef

help:
	@echo "============================================================"
	@echo "  welcom to entropy emacs language server archive project"
	@echo "============================================================"
	@echo ""
	@echo "MAKE options:"
	@echo ""
	@echo "- help: show this prompt buffer"
	@echo ""
	@echo "- all: generate language server executes according to current platform"

all:
	@git clean -xfd .
	@$(DOIT)
	@$(value DoExtra)
