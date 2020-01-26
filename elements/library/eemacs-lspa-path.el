;; * code
(defvar eemacs-lspa/path-project-root
  (expand-file-name
   "../../"
   (expand-file-name
    (file-name-directory load-file-name))))

(defvar eemacs-lspa/path-lspa-repos-root
  (expand-file-name
   "elements/lspa-repos"
   eemacs-lspa/path-project-root))

(defvar eemacs-lspa/path-lspa-recipes-root
  (expand-file-name
   "elements/recipes"
   eemacs-lspa/path-project-root))

;; * provide
(provide 'eemacs-lspa-path)
