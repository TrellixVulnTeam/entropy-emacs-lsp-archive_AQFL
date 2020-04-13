;;; eemacs-lspa-path.el --- eemacs lspa path varaible defination
;;
;; * Copyright (C) 2020-04-14  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; #+END_EXAMPLE
;;
;; * Commentary:
;;
;; This library given the variable defination for this project.
;;
;; * Configuration:
;;
;; Just load it.
;;
;; * Code:


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
