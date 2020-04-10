(require 'eemacs-lspa-subr)

(defvar eemacs-lspa/pypi-gnulinux_aarch64-loader-root
  (file-name-directory load-file-name))

(defvar eemacs-lspa/pypi-gnulinux_aarch64-wheels-root
  (expand-file-name
   "wheels"
   eemacs-lspa/pypi-gnulinux_aarch64-loader-root))

(defvar eemacs-lspa/pypi-gnulinux_aarch64-orig-exec-root
  (expand-file-name
   "bin"
   eemacs-lspa/pypi-gnulinux_aarch64-wheels-root))

(defvar eemacs-lspa/pypi-gnulinux_aarch64-caller-root
  (expand-file-name
   "callers"
   eemacs-lspa/pypi-gnulinux_aarch64-loader-root))

;; copy orig execs to callers folder
(cond ((and (file-exists-p eemacs-lspa/pypi-gnulinux_aarch64-caller-root)
         (eemacs-lspa/subr-noninteractive))
       (delete-directory eemacs-lspa/pypi-gnulinux_aarch64-caller-root t)
       (copy-directory eemacs-lspa/pypi-gnulinux_aarch64-orig-exec-root
                       eemacs-lspa/pypi-gnulinux_aarch64-caller-root))
      ((and (not (file-exists-p eemacs-lspa/pypi-gnulinux_aarch64-caller-root))
            (eemacs-lspa/subr-noninteractive))
       (copy-directory eemacs-lspa/pypi-gnulinux_aarch64-orig-exec-root
                       eemacs-lspa/pypi-gnulinux_aarch64-caller-root)))

(defvar eemacs-lspa/pypi-gnulinux_aarch64-lib-root
  (expand-file-name
   "lib/python3.8/site-packages"
   eemacs-lspa/pypi-gnulinux_aarch64-wheels-root))

(defun eemacs-lspa/pypi-gnulinux_aarch64-get-bins ()
  (let ((dir-list (eemacs-lspa/subr-list-dir-lite
                   eemacs-lspa/pypi-gnulinux_aarch64-caller-root))
        pyfiles)
    (dolist (item dir-list)
      (when (equal (car item) "F")
        (push (cdr item) pyfiles)))
    pyfiles))

(defun eemacs-lspa/pypi-gnulinux_aarch64-patch-bins ()
  (let ((pyfiles (eemacs-lspa/pypi-gnulinux_aarch64-get-bins)))
    (dolist (bin pyfiles)
      (eemacs-lspa/subr-pypi-patch-python-bin-for-pythonpath
       (expand-file-name bin)
       (expand-file-name
        eemacs-lspa/pypi-gnulinux_aarch64-lib-root)))))

(eemacs-lspa/subr-common-do-with-prompt
 "Patch pypi gnulinux aarch64 platform lsp archive"
 "Loading pypi gnulinux aarch64 platform lsp archive"
 :make-body
 ((progn
    (eemacs-lspa/pypi-gnulinux_aarch64-patch-bins)))
 :load-body
 ((eemacs-lspa/subr-add-path
   eemacs-lspa/pypi-gnulinux_aarch64-caller-root nil
   eemacs-lspa/pypi-gnulinux_aarch64-caller-root nil)))

(provide 'eemacs-lspa-pypi-prebuilt-gnulinux-aarch64-load)  
