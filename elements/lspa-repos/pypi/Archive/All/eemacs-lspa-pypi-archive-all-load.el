(require 'eemacs-lspa-subr)

(defvar eemacs-lspa/pypi-archive-all-loader-root
  (file-name-directory load-file-name))

(defvar eemacs-lspa/pypi-archive-all-wheels-root
  (expand-file-name "wheels" eemacs-lspa/pypi-archive-all-loader-root))

(defvar eemacs-lspa/pypi-archive-all-caller-root
  (expand-file-name "callers" eemacs-lspa/pypi-archive-all-loader-root))

(dolist (dir (list eemacs-lspa/pypi-archive-all-caller-root
                   eemacs-lspa/pypi-archive-all-wheels-root))
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defvar eemacs-lspa/pypi-archive-all-bin-root
  (expand-file-name
   "bin"
   eemacs-lspa/pypi-archive-all-wheels-root))

(defvar eemacs-lspa/pypi-archive-all-install-sh-file
  (expand-file-name
   "install.sh"
   eemacs-lspa/pypi-archive-all-loader-root))

(defvar eemacs-lspa/pypi-archive-all-install-cmd-file
  (expand-file-name
   "install.cmd"
   eemacs-lspa/pypi-archive-all-loader-root))

(eemacs-lspa/subr-common-do-with-prompt
 "Init pypi-archive-all lsp archive"
 "Loading eemacs-lspa pypi branch"
 :load-body
 ((eemacs-lspa/subr-add-path
   eemacs-lspa/pypi-archive-all-caller-root nil
   eemacs-lspa/pypi-archive-all-caller-root nil))
 :make-body
 ((eemacs-lspa/subr-add-batch-file
   "Installing all pypi packages"
   (if (eq system-type 'windows-nt)
       (format
        "
call %s
"
        eemacs-lspa/pypi-archive-all-install-cmd-file)
     (format
      "
set -e
bash %s
"
      eemacs-lspa/pypi-archive-all-install-sh-file)))))

(provide 'eemacs-lspa-npm-prebuilt-all-load)
