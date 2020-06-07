# * code
# ** pre
EMACS ?= emacs

DOIT := $(EMACS) -q --batch -l ./eemacs-lsp-archive-load.el

# ** os detecting
# OS detected method obtained by https://stackoverflow.com/questions/714100/os-detecting-makefile
ifeq '$(findstring ;,$(PATH))' ';'
    detected_OS := Windows
else
    detected_OS := $(shell uname 2>/dev/null || echo Unknown)
    detected_OS := $(patsubst CYGWIN%,Cygwin,$(detected_OS))
    detected_OS := $(patsubst MSYS%,MSYS,$(detected_OS))
    detected_OS := $(patsubst MINGW%,MSYS,$(detected_OS))
endif
# *** condition description
# ifeq ($(detected_OS),Windows)
# endif
# ifeq ($(detected_OS),Darwin)        # Mac OS X
# endif
# ifeq ($(detected_OS),Linux)
# endif
# ifeq ($(detected_OS),GNU)           # Debian GNU Hurd
# endif
# ifeq ($(detected_OS),GNU/kFreeBSD)  # Debian kFreeBSD
# endif
# ifeq ($(detected_OS),FreeBSD)
# endif
# ifeq ($(detected_OS),NetBSD)
# endif
# ifeq ($(detected_OS),DragonFly)
# endif
# ifeq ($(detected_OS),Haiku)
# endif
# ifeq ($(detected_OS),Msys)
# endif
# ifeq ($(detected_OS),Cygwin)
# endif


# ** defines
ifeq ($(detected_OS),Windows)
define DoExtra =
	cmd /c if exist elements/library/eemacs-lspa-install.cmd (call elements/library/eemacs-lspa-install.cmd)
endef
else
define DoExtra =
	bash -c "if [[ -f elements/library/eemacs-lspa-install.sh ]];then cd elements/library && bash eemacs-lspa-install.sh;fi"
endef
endif

ifeq ($(Eemacs_Lspa_Use_Platform),windows-nt)
define DoExtra =
	cmd /c if exist elements/library/eemacs-lspa-install.cmd (call elements/library/eemacs-lspa-install.cmd)
endef
endif

# ** rules
help:
	$(info ------------------------------------------------------------)
	$(info   welcom to entropy emacs language server archive project)
	$(info ------------------------------------------------------------)
	$(info )
	$(info This project require GNU/Make version larger than 4.0, if not, will messing your result)
	$(info )
	$(info MAKE options:)
	$(info )
	$(info - help:   show this prompt buffer)
	$(info )
	$(info - all:    generate language server executes according to current platform)
	$(info )
	$(info - create: create a new recipe)
	$(info )

emacs_version:
	@$(EMACS) --version

clean:	emacs_version
	$(info )
	@$(EMACS) -q --batch -l eemacs-lspa-clean-root.el


create: clean
	@$(EMACS) -q --batch -l eemacs-lsp-archive-create-recipe.el


ifeq ($(detected_OS),Unknown)
all:	clean
	$(info Unknow platform! Abort!)

else
all:    clean
	$(info Make on $(detected_OS) ...)
	@$(DOIT)
	@$(value DoExtra)
	@touch init
endif
