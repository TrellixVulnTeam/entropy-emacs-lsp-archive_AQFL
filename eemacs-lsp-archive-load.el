;;; eemacs lsp archive project
;;
;; * Copyright (C) date  author
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; Package-Version: 0.1.0
;; Created:       2020-01-21
;; Compatibility: GNU Emacs 25.3;
;; Package-Requires: ((emacs "25.3") (cl-lib "0.5"))
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

;; This project archived all [[https://github.com/c0001/entropy-emacs.git][entropy-emacs]] required Microsoft Language
;; Server implementations, and aimed for build and load all of them for
;; current working platform.

;; *Make:*

;; A simple make file stored in the root of this project, run it with
;; option =help= and =all= for quickly build for your machine
;; architecture, =aarch64=, =x86_64= are only supported ones, further
;; more, only operation system included in =Windows= and =gnu/linux= were
;; supported as so.

;; *load:*

;; The project was designed as the extension for GNU/emacs above(and
;; include) for 25.3. The init loader [[file:eemacs-lsp-archive-load.el][eemacs-lsp-archive-load.el]] can be
;; load by elisp function ~load~, that say:

;; #+begin_src elisp
;; (load "path-to-this-project/eemacs-lsp-archive-load.el")
;; #+end_src

;; Or add to this project root to your =load-path= and ~require~ it or
;; using config loader management like =use-package= to deferred load the
;; feature =eemacs-lsp-archive-load=.

;; ** Contribute

;; There's one elisp object =lspa-register=, whose car was its name and
;; the cdr was a plist called =lspa-rgister-tree=, name was a symbol for
;; indicate what it is, and the tree has three slot =:root=, =:prebuilt=
;; and the =:archive=. This project represent the language server archvie
;; as it, the object indicate where to find the archive and how to build
;; and load thus.

;; For the keys content formal and meaningful see below list table:

;; - =:root=

;;   The =:root= slot contained the lsp archive root path, its type of a
;;   path string.

;; - =:prebuilt=

;;   For cross-platform designation, a folder named =PreBuilt= must be
;;   stored under the =:root=, and all the subdir of it are dired as
;;   struct of =system-type/architecture= as its elements, so that one
;;   =lspa-register= is served for multi-platfom of multi-architecture.

;;   For represent the folder structure of =:prebuilt=, the value of this
;;   key was one alist whose car of each elements was the =system-type=
;;   e.g. 'windows-nt', 'gnu/linux', 'darwin' and so more recognized by
;;   elisp =system-type= const, the cdr was a alist whose car was the
;;   architecture, a symbol like 'x86_64', 'aarch64' etc and rest of it
;;   was a plist and now just has one key =:init=, the loader for the
;;   specified architecture, its a elisp file.

;; - =:archive=

;;   The same structure for =:prebuilt=, but for source installing
;;   aiming.  Folder =Archive= must be stored in the =:root= as what
;;   =:prebuilt= did.


;; For the alist of =:prebuilt= or =:archive= slot, can have the *all*
;; platform-and-architecture supported element whose car was 'all' and
;; the rest obey the specified architecture rest form. The *all* type
;; element means can be load for whatever platform and architecture.

;; *** Make and load prior

;; That see of the =lspa-register= slot =:prebuilt= and =:archive=, we
;; always prefer to find the prebuilt loader if not found then search the
;; archive loader for individual platform and machine architecture, if
;; not for all, warning hint for user and exit.

;; *** Recipes

;; For more benefit maintaining and developing =lspa-register=, we have
;; the convention for putting each =lspa-register= to a file called
;; =recipe= under the =elements/recipes= folder as what melpa do (if your
;; are a emacs package developer, you may know what [[https://melpa.org/][melpa]] is). That say
;; that all the =lspa-register= elisp form are the only content of its
;; recipe. The main loader this project will read them automatically by
;; making and loading.

;; So as for the contributor, put your recipe into the 'elements/recipes'
;; folder after your register folder built up, that's all.




;; * Code

;; ** preface
(defvar eemacs-lspa/project-root
  (expand-file-name
   (file-name-directory load-file-name)))

(defvar eemacs-lspa/project-elisp-library-root
  (expand-file-name "elements/library"
                    eemacs-lspa/project-root))

(add-hook 'load-path eemacs-lspa/project-elisp-library-root)

(require 'eemacs-lspa-path)
(require 'eemacs-lspa-subr)

;; ** Register
;; *** alias
(defvar eemacs-lspa/project-arch-alias
  '(("x64-based" x86_64)
    ("x86_64" x86_64)))

(defvar eemacs-lspa/project-arch-folder-alias
  '((x86_64 "x86_64")
    (aarch64 "aarch64")))

(defvar eemacs-lspa/project-platform-folder-alias
  '((gnu/linux "GnuLinux")
    (gnu "GnuHurd")
    (gnu/kfreebsd "GnuKfreebsd")
    (darwin "Darwin")
    (ms-dos "MsDos")
    (windows-nt "WindowsNT")
    (cygwin "Cygwin")))

;; *** register entries
(defvar eemacs-lspa/project-lspa-summary
  (eemacs-lspa/subr-read-recipes
   eemacs-lspa/path-lspa-recipes-root))

;; ** Register parse
;; *** library
;; **** Loader expand 
(defun eemacs-lspa/project-expand-loader-for-prebuilt-individual (elroot platform arch loader-name)
  (let ((pltfname (car (alist-get platform eemacs-lspa/project-platform-folder-alias)))
        (archfname (car (alist-get arch eemacs-lspa/project-arch-folder-alias))))
    (expand-file-name
     loader-name
     (expand-file-name
      archfname
      (expand-file-name
       pltfname
       (expand-file-name "PreBuilt" (expand-file-name elroot)))))))

(defun eemacs-lspa/project-expand-loader-for-prebuilt-all (elroot loader-name)
  (expand-file-name
   loader-name
   (expand-file-name
    "All"
    (expand-file-name
     "PreBuilt" (expand-file-name elroot)))))

(defun eemacs-lspa/project-expand-loader-for-archive-individual (elroot platform arch loader-name)
  (let ((pltfname (car (alist-get platform eemacs-lspa/project-platform-folder-alias)))
        (archfname (car (alist-get arch eemacs-lspa/project-arch-folder-alias))))
    (expand-file-name
     loader-name
     (expand-file-name
      archfname
      (expand-file-name
       pltfname
       (expand-file-name "Archive" (expand-file-name elroot)))))))

(defun eemacs-lspa/project-expand-loader-for-archive-all (elroot loader-name)
  (expand-file-name
   loader-name
   (expand-file-name
    "All"
    (expand-file-name
     "Archive" (expand-file-name elroot)))))


;; *** main
(defun eemacs-lspa/project-query-register (lspa-register)
  "return the loader or nil for unsupport various"
  (let* ((name (car lspa-register))
         (register-tree (cdr lspa-register))
         (register-root (expand-file-name (plist-get register-tree :root)
                                          eemacs-lspa/path-lspa-repos-root))
         (prebuilt-obj (plist-get register-tree :prebuilt))
         (archive-obj (plist-get register-tree :archive))
         (cur-platform system-type)
         (cur-arch
          (cond
           ((eq cur-platform 'window-nt)
            (let ((sysinfo))
              (setq sysinfo
                    (car (split-string
                          (nth 1
                               (split-string
                                (shell-command-to-string
                                 "systeminfo | findstr /R \"System.Type\"")
                                ":" t))
                          " " t)))
              (car (alist-get sysinfo eemacs-lspa/project-arch-alias))))
           (t
            (intern (replace-regexp-in-string
                     "\n" ""
                     (shell-command-to-string "uname -m")))))))
    (let* (builtin-support
           archive-support
           (reg-prebuilt-platform-all (alist-get 'all prebuilt-obj))
           (reg-prebuilt-platform (alist-get cur-platform prebuilt-obj))
           (reg-prebuilt-platform-arch (alist-get cur-arch reg-prebuilt-platform))
           reg-prebuilt-use-all
           (reg-archive-platform-all (alist-get 'all archive-obj))
           (reg-archive-platform (alist-get cur-platform archive-obj))
           (reg-archive-platform-arch (alist-get cur-arch reg-archive-platform))
           reg-archive-use-all)

      (catch :exit
        (unless (or (and reg-prebuilt-platform reg-prebuilt-platform-arch)
                    (and reg-prebuilt-platform-all
                         (setq reg-prebuilt-use-all t)))
          (throw :exit nil))
        (if reg-prebuilt-use-all
            (setq builtin-support
                  (eemacs-lspa/project-expand-loader-for-prebuilt-all
                   register-root (plist-get reg-prebuilt-platform-all :init)))
          (setq builtin-support (eemacs-lspa/project-expand-loader-for-prebuilt-individual
                                 register-root cur-platform cur-arch
                                 (plist-get reg-prebuilt-platform-arch :init)))))
      (catch :exit
        (unless (or (and reg-archive-platform reg-archive-platform-arch)
                    (and reg-archive-platform-all
                         (setq reg-archive-use-all t)))
          (throw :exit nil))
        (if reg-archive-use-all
            (setq archive-support
                  (eemacs-lspa/project-expand-loader-for-archive-all
                   register-root (plist-get reg-archive-platform-all :init)))
          (setq archive-support (eemacs-lspa/project-expand-loader-for-archive-individual
                                 register-root cur-platform cur-arch
                                 (plist-get reg-archive-platform-arch :init)))))
      (or builtin-support archive-support))))


;; * provide
(defvar eemacs-lspa/project-loaders-obtained nil)
(defvar eemacs-lspa/project-loaders-missing nil)

(let (rtn)
  (dolist (item eemacs-lspa/project-lspa-summary)
    (push (eemacs-lspa/project-query-register item) rtn))
  (setq eemacs-lspa/project-loaders-obtained rtn)
  (dolist (loader rtn)
    (unless (file-exists-p loader)
      (add-to-list 'eemacs-lspa/project-loaders-missing loader)))
  (when eemacs-lspa/project-loaders-missing
    (error
     "eemacs lspa registed loader missing, 
please check variable `eemacs-lspa/project-loaders-missing'"))
  (dolist (loader eemacs-lspa/project-loaders-obtained)
    (load loader))
  (when (eemacs-lspa/subr-noninteractive)
    (with-current-buffer
        (find-file-noselect
         (expand-file-name "init" eemacs-lspa/project-root))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (save-buffer)
        (kill-buffer)))))

(provide 'eemacs-lsp-archive-load)
