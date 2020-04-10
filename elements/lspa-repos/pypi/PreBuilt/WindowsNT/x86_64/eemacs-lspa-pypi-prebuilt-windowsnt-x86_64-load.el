(require 'cl-lib)
(require 'eemacs-lspa-subr)

(defvar eemacs-lspa/pypi-win64-root
  (file-name-directory load-file-name))

(defvar eemacs-lspa/pypi-win64-wheels-root
  (expand-file-name "wheels"
                    eemacs-lspa/pypi-win64-root))

(defvar eemacs-lspa/pypi-win64-callers-root
  (expand-file-name "callers"
                    eemacs-lspa/pypi-win64-root))

(unless (file-exists-p eemacs-lspa/pypi-win64-callers-root)
  (mkdir eemacs-lspa/pypi-win64-callers-root t))

(defvar eemacs-lspa/pypi-win64-bin-path
  (expand-file-name "Scripts"
                    eemacs-lspa/pypi-win64-wheels-root))

(defvar eemacs-lspa/pypi-win64-lib-path
  (expand-file-name "Lib/site-packages"
                    eemacs-lspa/pypi-win64-wheels-root))

(defun eemacs-lspa/pypi-win64-get-exe-bins ()
  (let ((items (eemacs-lspa/subr-list-dir-lite
                eemacs-lspa/pypi-win64-bin-path)))
    (cl-delete nil
             (mapcar
              (lambda (item)
                (let ((type (car item))
                      (fname (cdr item)))
                  (when (and (string= "F" type)
                             (string-match-p "\\.exe$" fname))
                    fname)))
              items))))

(defun eemacs-lspa/pypi-win64-get-py-bins ()
  (let ((items (eemacs-lspa/subr-list-dir-lite
                eemacs-lspa/pypi-win64-bin-path)))
    (cl-delete nil
             (mapcar
              (lambda (item)
                (let ((type (car item))
                      (fname (cdr item)))
                  (when (and (string= "F" type)
                             (string-match-p "\\.py$" fname))
                    fname)))
              items))))

(defun eemacs-lspa/pypi-win64-gen-cmds ()
  (let ((w32-bins (eemacs-lspa/pypi-win64-get-exe-bins)))
    (dolist (w32bin w32-bins)
      (let ((w32cmd (expand-file-name
                     (concat (file-name-base w32bin) ".cmd")
                     eemacs-lspa/pypi-win64-callers-root)))
        (eemacs-lspa/subr-pypi-gen-python-w32-cmd-bin
         w32cmd
         (expand-file-name w32bin)
         (expand-file-name eemacs-lspa/pypi-win64-lib-path))))))


(defun eemacs-lspa/pypi-win64-patch-py-bins ()
  (let ((pyfiles (eemacs-lspa/pypi-win64-get-py-bins)))
    (dolist (item pyfiles)
      (eemacs-lspa/subr-pypi-patch-python-bin-for-pythonpath
       (expand-file-name item)
       (expand-file-name eemacs-lspa/pypi-win64-lib-path)))))

(eemacs-lspa/subr-common-do-with-prompt
 "Generating win64 pypi callers"
 "Loading win64 pypi lsp archive"
 :make-body
 ((progn
    (eemacs-lspa/pypi-win64-gen-cmds)
    (eemacs-lspa/pypi-win64-patch-py-bins)))
 :load-body
 ((eemacs-lspa/subr-add-path
   eemacs-lspa/pypi-win64-callers-root nil
   eemacs-lspa/pypi-win64-callers-root nil)))

(provide 'eemacs-lspa-pypi-prebuilt-windowsnt-x86_64-load)
