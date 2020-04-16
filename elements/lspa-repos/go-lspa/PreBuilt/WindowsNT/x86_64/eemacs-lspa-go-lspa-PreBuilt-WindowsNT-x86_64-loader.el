;; * Code
;; ** Require
(require 'eemacs-lspa-subr)
;; ** Variable Declaration
(defvar eemacs-lspa/go-lspa-PreBuilt-WindowsNT-x86_64-loader--root (file-name-directory (expand-file-name load-file-name)))
(defvar eemacs-lspa/go-lspa-PreBuilt-WindowsNT-x86_64-loader--gopls-dir
  (expand-file-name
   "gopls"
   eemacs-lspa/go-lspa-PreBuilt-WindowsNT-x86_64-loader--root))

;; ** Main
;; Use `eemacs-lspa/subr-common-do-with-prompt' for export loader
(eemacs-lspa/subr-common-do-with-prompt
 "Init gopls ..."
 "Add gopls to path ..."
 :load-body
 ((progn
    (eemacs-lspa/subr-add-path
     eemacs-lspa/go-lspa-PreBuilt-WindowsNT-x86_64-loader--gopls-dir nil
     eemacs-lspa/go-lspa-PreBuilt-WindowsNT-x86_64-loader--gopls-dir nil))))

;; * Provide
(provide 'eemacs-lspa-go-lspa-PreBuilt-WindowsNT-x86_64-loader)
