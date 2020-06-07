(defvar eemacs-lspa/clr--call-root
  (file-name-directory
   (expand-file-name load-file-name)))

(defvar eemacs-lspa/clr--hosted-files
  (let ((file (expand-file-name "project-files.txt"
                                eemacs-lspa/clr--call-root))
        content)
    (with-current-buffer (find-file-noselect file t)
      (setq content (buffer-substring-no-properties (point-min) (point-max))
            content (split-string content "\n" t)))
    content))

(defvar eemacs-lspa/clr--exclusion-files nil)

(defun eemacs-lspa/clr--get-project-files-currently ()
  (let ((root-dir eemacs-lspa/clr--call-root)
        rtn)
    (setq rtn
          (directory-files-recursively
           root-dir
           ""
           nil
           (lambda (x)
             (if (string-match-p
                  (format "%s%s"
                          (regexp-quote root-dir)
                          "\\.git")
                  x)
                 nil
               t)))
          rtn
          (remove (expand-file-name ".git" root-dir) rtn)
          rtn
          (mapcar
           (lambda (x)
             (replace-regexp-in-string
              "[/\\\\]$"
              ""
              (replace-regexp-in-string (regexp-quote root-dir) "" x)))
           rtn))))


(message "Start make cleanup ...")

(setq eemacs-lspa/clr--exclusion-files
      (let ((exl-files
             (mapcar (lambda (x) (and (not (member x eemacs-lspa/clr--hosted-files)) x))
                     (eemacs-lspa/clr--get-project-files-currently))))
        (delete nil exl-files)))

(dolist (file eemacs-lspa/clr--exclusion-files)
  (let* ((abs-path (expand-file-name file eemacs-lspa/clr--call-root))
         (file-dir (file-name-directory abs-path)))
    (when (file-exists-p abs-path)
      (message "Cleanup for generated file: %s" file)
      (delete-file abs-path)
      (let ((dir-files (delete nil
                               (mapcar (lambda (x) (and (not (string-match-p "^\\(\\.\\|\\.\\.\\)$" x))
                                                        x))
                                       (directory-files file-dir)))))
        (when (null dir-files)
          (message "Cleanup for rest empty dir: %s" file-dir)
          (delete-directory file-dir))))))

(message "Cleanup done!")

(provide 'eemacs-lspa-clean-root)
