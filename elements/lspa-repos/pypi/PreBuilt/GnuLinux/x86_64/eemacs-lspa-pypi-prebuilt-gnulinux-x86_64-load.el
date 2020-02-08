(require 'eemacs-lspa-subr)

(defvar eemacs-lspa/pypi-gnulinuxx64-loader-root
  (file-name-directory load-file-name))

(defvar eemacs-lspa/pypi-gnulinuxx64-wheels-root
  (expand-file-name
   "wheels"
   eemacs-lspa/pypi-gnulinuxx64-loader-root))

(defvar eemacs-lspa/pypi-gnulinuxx64-orig-exec-root
  (expand-file-name
   "bin"
   eemacs-lspa/pypi-gnulinuxx64-wheels-root))

(defvar eemacs-lspa/pypi-gnulinuxx64-caller-root
  (expand-file-name
   "callers"
   eemacs-lspa/pypi-gnulinuxx64-loader-root))

;; copy orig execs to callers folder
(cond ((and (file-exists-p eemacs-lspa/pypi-gnulinuxx64-caller-root)
         (eemacs-lspa/subr-noninteractive))
       (delete-directory eemacs-lspa/pypi-gnulinuxx64-caller-root t)
       (copy-directory eemacs-lspa/pypi-gnulinuxx64-orig-exec-root
                       eemacs-lspa/pypi-gnulinuxx64-caller-root))
      ((and (not (file-exists-p eemacs-lspa/pypi-gnulinuxx64-caller-root))
            (eemacs-lspa/subr-noninteractive))
       (copy-directory eemacs-lspa/pypi-gnulinuxx64-orig-exec-root
                       eemacs-lspa/pypi-gnulinuxx64-caller-root)))

(defvar eemacs-lspa/pypi-gnulinuxx64-lib-root
  (expand-file-name
   "lib/python3.8/site-packages"
   eemacs-lspa/pypi-gnulinuxx64-wheels-root))

(defun eemacs-lspa/pypi-gnulinuxx64-get-bins ()
  (let ((dir-list (eemacs-lspa/subr-list-dir-lite
                   eemacs-lspa/pypi-gnulinuxx64-caller-root))
        pyfiles)
    (dolist (item dir-list)
      (when (equal (car item) "F")
        (push (cdr item) pyfiles)))
    pyfiles))

(defun eemacs-lspa/pypi-gnulinuxx64-patch-bins ()
  (let ((pyfiles (eemacs-lspa/pypi-gnulinuxx64-get-bins)))
    (dolist (bin pyfiles)
      (eemacs-lspa/subr-pypi-patch-python-bin-for-pythonpath
       (expand-file-name bin)
       (expand-file-name
        eemacs-lspa/pypi-gnulinuxx64-lib-root)))))

(eemacs-lspa/subr-common-print
 "Paching gnulinux x64 pypi lsp archive"
 "Loading gnulinux x64 pypi lsp archive"
 :make-body
 ((progn
    (eemacs-lspa/pypi-gnulinuxx64-patch-bins)))
 :load-body
 ((eemacs-lspa/subr-add-path
   eemacs-lspa/pypi-gnulinuxx64-caller-root nil
   eemacs-lspa/pypi-gnulinuxx64-caller-root nil)))

(provide 'eemacs-lspa-pypi-prebuilt-gnulinux-x86_64-load)  
