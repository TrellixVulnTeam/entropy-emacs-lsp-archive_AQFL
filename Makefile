DOIT=emacs -q --batch -l ./eemacs-lsp-archive-load.el

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
	@$(DOIT)
