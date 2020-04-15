;; * Code
;; ** Require
(require 'eemacs-lspa-subr)
;; ** Variable Declaration
(defvar eemacs-lspa/pwsh-lspa-PreBuilt-All-loader--root (file-name-directory (expand-file-name load-file-name)))

(defvar eemacs-lspa/pwsh-lspa-PreBuilt-All-loader--pwsh-lspa-dir
  (expand-file-name "PowerShellEditorServices"
                    eemacs-lspa/pwsh-lspa-PreBuilt-All-loader--root))

;; ** Main
;; Use `eemacs-lspa/subr-common-do-with-prompt' for export loader

(eemacs-lspa/subr-common-do-with-prompt
 "Init powershell-language-server ..."
 "Add powershell-language-server to path ..."
 :load-body
 ((progn
    (setq lsp-pwsh-dir
          eemacs-lspa/pwsh-lspa-PreBuilt-All-loader--pwsh-lspa-dir))))

;; * Provide
(provide 'eemacs-lspa-pwsh-lspa-PreBuilt-All-loader)
