(require 'eemacs-lspa-subr)

(defvar eemacs-lspa/npm-archive-all-loader-root
  (file-name-directory load-file-name))

(defvar eemacs-lspa/npm-archive-all-modules-root
  (expand-file-name
   "node_modules"
   eemacs-lspa/npm-archive-all-loader-root))

(defvar eemacs-lspa/npm-archive-all-bin-root
  (expand-file-name
   ".bin"
   eemacs-lspa/npm-archive-all-modules-root))

(defvar eemacs-lspa/npm-archive-all-install-sh-file
  (expand-file-name
   "install.sh"
   eemacs-lspa/npm-archive-all-loader-root))

(eemacs-lspa/subr-common-do-with-prompt
 "Init npm-archive-all lsp archive"
 "Loading eemacs-lspa npm branch"
 :load-body
 ((eemacs-lspa/subr-add-path
   eemacs-lspa/npm-archive-all-bin-root nil
   eemacs-lspa/npm-archive-all-bin-root nil))
 :make-body
 ((eemacs-lspa/subr-add-batch-file
   "Installing all node packages"
   (format
    "
set -e
bash %s
"
    eemacs-lspa/npm-archive-all-install-sh-file))))

(provide 'eemacs-lspa-npm-prebuilt-all-load)
