;; * Code
;; ** Require
(require 'eemacs-lspa-subr)
;; ** Variable Declaration
(defvar eemacs-lspa/java-lspa-Prebuilt-All-loader--root (file-name-directory (expand-file-name load-file-name)))

(defvar eemacs-lspa/java-lspa-Prebuilt-All-loader--java-lsp-root
  (expand-file-name "java-language-server"
                    eemacs-lspa/java-lspa-Prebuilt-All-loader--root))

;; ** Main
;; Use `eemacs-lspa/subr-common-do-with-prompt' for export loader


(eemacs-lspa/subr-common-do-with-prompt
 "Init java-language-server ..."
 "Add path for java-language-server ..."
 :load-body
 ((setq lsp-java-server-install-dir
        eemacs-lspa/java-lspa-Prebuilt-All-loader--java-lsp-root)))



;; * Provide
(provide 'eemacs-lspa-java-lspa-Prebuilt-All-loader)
