(require 'eemacs-lspa-subr)

(defvar eemacs-lspa/npm-all-loader-root
  (file-name-directory load-file-name))

(defvar eemacs-lspa/npm-all-modules-root
  (expand-file-name
   "node_modules"
   eemacs-lspa/npm-all-loader-root))

(defvar eemacs-lspa/npm-all-bin-root
  (expand-file-name
   ".bin"
   eemacs-lspa/npm-all-modules-root))

(eemacs-lspa/subr-common-do-with-prompt
 "Init npm-prebuilt-all lsp archive"
 "Loading eemacs-lspa npm branch"
 :load-body
 ((eemacs-lspa/subr-add-path
   eemacs-lspa/npm-all-bin-root nil
   eemacs-lspa/npm-all-bin-root nil)))

(provide 'eemacs-lspa-npm-prebuilt-all-load)


  
