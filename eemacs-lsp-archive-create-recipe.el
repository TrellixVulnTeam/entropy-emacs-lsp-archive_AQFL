;;; eemacs-lspa-archive-create.el --- Create a template eemacs-lspa recipe
;;
;; * Copyright (C) 20200415  Entropy
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

;; This package built with aim to creat a template entropy-emacs
;; language server archive.

;; * Configuration:
;;
;; Do not manuually load this file, its only used for batch mode.
;;
;; * code

;; ** preface
(defvar eemacs-lspa/cr-root
  (expand-file-name
   (file-name-directory load-file-name)))

(defvar eemacs-lspa/cr-elisp-library-root
  (expand-file-name "elements/library"
                    eemacs-lspa/cr-root))

(add-hook 'load-path eemacs-lspa/cr-elisp-library-root)

(require 'eemacs-lspa-path)
(require 'eemacs-lspa-subr)

;; ** main

(defun eemacs-lspa/cr-gen-loader-prefix
    (recipe-name archive-type platform architecture)
  (if (string= platform "All")
      (format "eemacs-lspa/%s-%s-%s-loader-"
              recipe-name archive-type platform)
    (format "eemacs-lspa/%s-%s-%s-%s-loader-"
            recipe-name archive-type platform architecture)))

(defun eemacs-lspa/cr-gen-loader-filename
    (recipe-name archive-type platform architecture)
  (if (string= platform "All")
      (format "eemacs-lspa-%s-%s-%s-loader.el"
              recipe-name archive-type platform)
    (format "eemacs-lspa-%s-%s-%s-%s-loader.el"
            recipe-name archive-type platform architecture)))

(defun eemacs-lspa/cr-gen-loader-location-name
    (recipe-name archive-type platform architecture)
  (if (string= platform "All")
      (format "%s/%s/%s"
              recipe-name archive-type platform)
    (format "%s/%s/%s/%s"
            recipe-name archive-type platform architecture)))

(defun eemacs-lspa/cr-gen-fake-recipe-file-path (recipe-name)
  (expand-file-name
   (format "%s" recipe-name)
   eemacs-lspa/path-lspa-recipes-root))

(defun eemacs-lspa/cr-gen-loader-skeleton
    (recipe-name archive-type platform architecture)
  (let* ((inhibit-read-only t)
         (loader-prefix
          (eemacs-lspa/cr-gen-loader-prefix
           recipe-name archive-type platform architecture))
         (location
          (expand-file-name
           (eemacs-lspa/cr-gen-loader-location-name
            recipe-name archive-type platform architecture)
           eemacs-lspa/path-lspa-repos-root))
         (filename
          (expand-file-name
           (eemacs-lspa/cr-gen-loader-filename
            recipe-name archive-type platform architecture)
           location)))
    (make-directory location 'create-parent)
    (with-current-buffer (find-file-noselect filename)
      (erase-buffer)
      (insert "\;\; * Code\n")
      (insert "\;\; ** Require\n")
      (insert "(require 'eemacs-lspa-subr)\n")
      (insert "\;\; ** Variable Declaration\n")
      (insert (format "(defvar %s-root (file-name-directory (expand-file-name load-file-name)))\n"
                      loader-prefix))
      (insert "\;\; ** Main\n")
      (insert "\;\; Use `eemacs-lspa/subr-common-do-with-prompt' for export loader\n")
      (insert "\;\; * Provide\n")
      (insert (format "(provide '%s)\n"
                      (file-name-base filename)))
      (save-buffer))
    (concat (file-name-base filename) ".el")))


(defun eemacs-lspa/cr-merge-recipe-pre-plist (recipe-pre-plist)
  (let* ((do-list recipe-pre-plist) rtn)
    (let ((end-pt (- (length do-list) 1))
          categorized-list
          matched-pts
          (cur-pt 0)
          (judge-func
           (lambda (x)
             (and (listp x)
                  (> (length x) 1)
                  (not (null (cadr x)))
                  (listp (cadr x))))))
      (while (<= cur-pt end-pt)
        (let* ((referrence-element (nth cur-pt do-list))
               cur-group
               cur-key
               loop-pt)
          (when (not (member cur-pt matched-pts))
            (if (not (funcall judge-func referrence-element))
                (progn (push (cons cur-pt referrence-element) categorized-list)
                       (push cur-pt matched-pts))
              (setq cur-group (cdr referrence-element))
              (setq cur-key (car referrence-element))
              (setq loop-pt (+ cur-pt 1))
              (while (<= loop-pt end-pt)
                (when (not (member loop-pt matched-pts))
                  (let* ((loop-refer (nth loop-pt do-list))
                         loop-key
                         loop-rest)
                    (if (funcall judge-func loop-refer)
                        (progn
                          (setq loop-key (car loop-refer))
                          (setq loop-rest (cdr loop-refer))
                          (when (eq cur-key loop-key)
                            (setq cur-group (append cur-group loop-rest))
                            (push loop-pt matched-pts)))
                      (push (cons loop-pt loop-refer) categorized-list)
                      (push loop-pt matched-pts))))
                (cl-incf loop-pt))
              (push (cons cur-pt (list cur-key cur-group)) categorized-list)))
          (cl-incf cur-pt)))
      (setq categorized-list
            (mapcar (lambda (x) (cdr x))
                    (sort categorized-list
                          (lambda (x y)
                            (< (car x) (car y))))))
      (let ((pt 0))
        (dolist (el categorized-list)
          (if (and (listp el)
                   (not (null (cadr el)))
                   (listp (cadr el)))
              (push (list (car el) (eemacs-lspa/cr-merge-recipe-pre-plist (cadr el)))
                    rtn)
            (push el rtn)))
        (reverse rtn)))))

(defun eemacs-lspa/cr-gen-fake-recipe (recipe-symbol recipe-pre-plist)
  (let ((merged-plist
         (eemacs-lspa/cr-merge-recipe-pre-plist
          recipe-pre-plist))
        processing-one)
    (dolist (el merged-plist)
      (setq processing-one
            (append processing-one
                    el)))
    (append
     (list recipe-symbol :root (format "%s" recipe-symbol))
     processing-one)))

(let* ((recipe-name
        (let ((input (read-string "Input Recipe Name: ")))
          (when (string= input "")
            (error "Can not use empty recipe name!"))
          (replace-regexp-in-string
           " " "-" input)))
       (archive-type-list '(("PreBuilt" :prebuilt) ("Archive" :archive)))
       (platform-list (mapcar (lambda (x) (list (cadr x) (car x)))
                              eemacs-lspa/subr-platform-folder-alist))
       (architecture-list (mapcar (lambda (x) (list (cadr x) (car x)))
                                  eemacs-lspa/subr-architecture-folder-alist))
       recipe-pre-plist)
  (dolist (archive-type-ref archive-type-list)
    (dolist (platform-ref platform-list)
      (if (string= (car platform-ref) "All")
          (push
           `(,(cadr archive-type-ref)
             (all :init
                  ,(eemacs-lspa/cr-gen-loader-skeleton
                    recipe-name
                    (car archive-type-ref)
                    (car platform-ref)
                    nil)))
           recipe-pre-plist)
        (dolist (architecture-ref architecture-list)
          (push
           `(,(cadr archive-type-ref)
             (,(cadr platform-ref)
              (,(cadr architecture-ref)
               :init
               ,(eemacs-lspa/cr-gen-loader-skeleton
                 recipe-name
                 (car archive-type-ref)
                 (car platform-ref)
                 (car architecture-ref)))))
           recipe-pre-plist)))))
  (setq recipe-pre-plist (reverse recipe-pre-plist))
  (let ((recipe-template (eemacs-lspa/cr-gen-fake-recipe (intern recipe-name) recipe-pre-plist))
        (recipe-file-path (eemacs-lspa/cr-gen-fake-recipe-file-path recipe-name)))
    (with-current-buffer (find-file-noselect recipe-file-path)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (print recipe-template (current-buffer))
        (goto-char (point-min))
        (insert
         (format
          "\;\; -*- mode: emacs-lisp\; -*-
\;\;
\;\; This is the recipe tempate content for recipe `%s', it gives all
\;\; platform archive former which you can choose to enable those you
\;\; want and comment the other not be needed.
\;\;
\;\; * Code
"
          recipe-name))
        (save-buffer)))
    (eemacs-lspa/subr-prettify-elisp-file
     recipe-file-path))
  (message "")
  (message "Succeed to create recipe template for name `%s'." recipe-name))
