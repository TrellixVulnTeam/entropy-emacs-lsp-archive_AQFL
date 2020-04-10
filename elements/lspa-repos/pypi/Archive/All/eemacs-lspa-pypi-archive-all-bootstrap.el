(require 'cl-lib)

(defvar eemacs-lspa/pypi-archive-all-after-install-root
  (file-name-directory load-file-name))

(load
 (expand-file-name "../../../../library/eemacs-lspa-subr.el"
                   eemacs-lspa/pypi-archive-all-after-install-root))

(defvar eemacs-lspa/pypi-archive-all-after-install-wheels-root
  (expand-file-name "wheels"
                    eemacs-lspa/pypi-archive-all-after-install-root))

(defvar eemacs-lspa/pypi-archive-all-after-install-callers-root
  (expand-file-name "callers"
                    eemacs-lspa/pypi-archive-all-after-install-root))

(defvar eemacs-lspa/pypi-archive-all-after-install-bin-path
  (expand-file-name "bin"
                    eemacs-lspa/pypi-archive-all-after-install-wheels-root))

(defvar eemacs-lspa/pypi-archive-all-after-install-lib-path
  (expand-file-name "lib"
                    eemacs-lspa/pypi-archive-all-after-install-wheels-root))

(defun eemacs-lspa/pypi-archive-all-after-install-get-exe-bins ()
  (let ((items (eemacs-lspa/subr-list-dir-lite
                eemacs-lspa/pypi-archive-all-after-install-bin-path)))
    (cl-delete nil
             (mapcar
              (lambda (item)
                (let ((type (car item))
                      (fname (cdr item)))
                  (when (and (string= "F" type)
                             (string-match-p "\\.exe$" fname))
                    fname)))
              items))))

(defun eemacs-lspa/pypi-archive-all-after-install-get-py-bins (&optional non-suffix bin-path)
  (let ((items (eemacs-lspa/subr-list-dir-lite
                (or bin-path eemacs-lspa/pypi-archive-all-after-install-bin-path))))
    (cl-delete nil
             (mapcar
              (lambda (item)
                (let ((type (car item))
                      (fname (cdr item)))
                  (when (and (string= "F" type)
                             (or non-suffix (string-match-p "\\.py$" fname)))
                    fname)))
              items))))

(defun eemacs-lspa/pypi-archive-all-after-install-gen-cmds ()
  (let ((w32-bins (eemacs-lspa/pypi-archive-all-after-install-get-exe-bins)))
    (dolist (w32bin w32-bins)
      (let ((w32cmd (expand-file-name
                     (concat (file-name-base w32bin) ".cmd")
                     eemacs-lspa/pypi-archive-all-after-install-callers-root)))
        (eemacs-lspa/subr-pypi-gen-python-w32-cmd-bin
         w32cmd
         (expand-file-name w32bin)
         (expand-file-name eemacs-lspa/pypi-archive-all-after-install-lib-path))))))


(defun eemacs-lspa/pypi-archive-all-after-install-patch-py-bins (&optional non-suffix bin-path)
  (let ((pyfiles (eemacs-lspa/pypi-archive-all-after-install-get-py-bins non-suffix bin-path)))
    (dolist (item pyfiles)
      (eemacs-lspa/subr-pypi-patch-python-bin-for-pythonpath
       (expand-file-name item)
       (expand-file-name eemacs-lspa/pypi-archive-all-after-install-lib-path)))))


(let ((w32-exes (eemacs-lspa/pypi-archive-all-after-install-get-exe-bins))
      (dotpys (eemacs-lspa/pypi-archive-all-after-install-get-py-bins)))

  (message "

Patching py bins ......")

  (cond ((and (null w32-exes)
              dotpys)
         (eemacs-lspa/pypi-archive-all-after-install-patch-py-bins))
        ((and (null dotpys)
              w32-exes)
         (eemacs-lspa/pypi-archive-all-after-install-gen-cmds))
        ((and dotpys
              w32-exes)
         (eemacs-lspa/pypi-archive-all-after-install-patch-py-bins)
         (eemacs-lspa/pypi-archive-all-after-install-gen-cmds))
        (t
         (when (file-exists-p eemacs-lspa/pypi-archive-all-after-install-callers-root)
           (delete-directory eemacs-lspa/pypi-archive-all-after-install-callers-root t))
         (copy-directory eemacs-lspa/pypi-archive-all-after-install-bin-path
                         eemacs-lspa/pypi-archive-all-after-install-callers-root
                         nil nil t)
         (eemacs-lspa/pypi-archive-all-after-install-patch-py-bins
          t
          eemacs-lspa/pypi-archive-all-after-install-callers-root)))
  (message "

Patching py bins done!

"
           ))

(provide 'eemacs-lspa-pypi-archive-all-bootstrap)
