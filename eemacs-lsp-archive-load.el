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
;; :PROPERTIES:
;; :CUSTOM_ID: h-8c8344df-e341-4183-9e06-cec26bd8bb43
;; :END:

;; There's one elisp object =lspa-register=, whose car was its name and
;; the cdr was a plist called =lspa-rgister-tree=, name was a symbol for
;; indicate what it is, and the tree has three slot =:root=, =:prebuilt=
;; and the =:archive=. This project represent the language server archvie
;; as it, the object indicate where to find the archive and how to build
;; and load thus.

;; For the keys content formal of =lspa-register-tree= and meaningful
;; see below list table:

;; 1. =:root=

;;    The =:root= slot contained the lsp archive root path, its type of a
;;    path string.

;; 2. =:prebuilt=

;;    For cross-platform designation, a folder named =PreBuilt= must
;;    be stored under the =:root=, and all the subdir of it are
;;    hosted as struct of =system-type/architecture= as its elements,
;;    so that one =lspa-register= is served for multi-platfom of
;;    multi-architecture.

;;    For represent the folder structure, the value of this key was
;;    one alist whose car of each elements was the =system-type=
;;    symbol e.g. 'windows-nt', 'gnu/linux', 'darwin' and so more
;;    recognized by elisp =system-type= const, the cdr of thus was a
;;    alist whose each element's car was the architecture, a symbol
;;    like 'x86_64', 'aarch64' etc and rest of it was a plist called
;;    =lspa-manager= which has keys for:
;;    1) =:init=, the loader for the specified architecture, its a
;;       elisp file for both run the building in *non-interactive*
;;       i.e. the batch-mode ENV and the loading infrastructure in
;;       interactive ENV.

;; 3. =:archive=

;;    The same structure for =:prebuilt=, but for source installing
;;    aiming.  Folder =Archive= must be stored in the =:root= as what
;;    =:prebuilt= did.


;; For the alist of =:prebuilt= or =:archive= slot, can have the
;; *all* platform-and-architecture supported element whose car was
;; 'all' and the rest form as same as the plist hosted in the
;; architecture slot i.e. the =lspa-manager=. The *all* type element
;; means can be load for _whatever platform and architecture_.

;; *** =lspa-register= sub-folder naming convention
;; :PROPERTIES:
;; :CUSTOM_ID: h-58d05c1a-62a2-44c4-be4f-1de662fe190b
;; :END:

;; The instance of a =lspa-register= was a folder with specific
;; folder structure, the sub-folders' distribution are commonly
;; reflecting with the =lspa-register= data nesting structure.

;; For building one =lspa-register= instance, the root of the archive
;; should (but no necessary) name as the =lspa-register='s name. And
;; then, the following three sub-folder are "Prebuilt", "Archive" and
;; "All", as above mentioned that they are optionally built with your
;; specification.

;; The important naming convention are those platform and
;; architecture sub-folders under those second hierarchy folder:

;; - For platform :: see the alist of =eemacs-lspa/project-platform-folder-alias=
;; - For architecture :: see the alist of =eemacs-lspa/project-arch-folder-alias=


;; Thus for a expample, we made a python-language-server
;; =lspa-register= instance:

;; #+begin_example
;;   + python-language-server
;;     + Prebuilt
;;       + WindowsNT
;;         - x86_64
;;       + GnuLinux
;;         - x86_64
;;         - aarch64
;;     - Archive
;; #+end_example

;; For the place hosting each =lspa-register= instance, please see
;; below sections. ([[h-8960a582-196b-44d4-ad49-bbf74cc943d6][recipe]])

;; *** Make and load prior
;; :PROPERTIES:
;; :CUSTOM_ID: h-7f5311d9-9ff0-4cb4-96f0-8775fd135246
;; :END:

;; That see of the =lspa-register= slot =:prebuilt= and =:archive=, we
;; always prefer to find the prebuilt loader if not found then search the
;; archive loader for individual platform and machine architecture, if
;; not for all, warning hint for user and exit.

;; *** Recipes
;; :PROPERTIES:
;; :CUSTOM_ID: h-8960a582-196b-44d4-ad49-bbf74cc943d6
;; :END:

;; For more benefit maintaining and developing =lspa-register=, we have
;; the convention for putting each =lspa-register= to a file called
;; =recipe= under the =elements/recipes= folder as what melpa do (if your
;; are a emacs package developer, you may know what [[https://melpa.org/][melpa]] is). That say
;; that all the =lspa-register= elisp form are the only content of its
;; recipe. The main loader this project will read them automatically by
;; making and loading.

;; So as for the contributor, put your recipe into the
;; =eemacs-lspa/path-lspa-recipes-root= folder after your register
;; folder built up, that's all.

;; Thus for a expample, we made a python-language-server
;; =lspa-register= recipe instance:

;; #+begin_src emacs-lisp
;;   (python-language-server
;;    :root
;;    "python-language-server"
;;    :prebuilt
;;    ((gnu/linux
;;      (x86_64 :init "eemacs-lspa-pyls-prebuilt-gnulinux-x86_64-load.el")
;;      (aarch64 :init "eemacs-lspa-pyls-prebuilt-gnulinux-aarch64-load.el"))
;;     (windows-nt
;;      (x86_64 :init "eemacs-lspa-pyls-prebuilt-windowsnt-x86_64-load.el")))
;;    :archive nil)
;; #+end_src

;; As see the sample, we use the abbreviation path for the archvie
;; root, which will be automatically expanding with
;; =eemacs-lspa/path-lspa-repos-root=.

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
;; ***** Prebuilt
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

;; ***** Archive
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


;; **** optional env condition detected

(defvar eemacs-lspa/project-force-use-archive nil)
(defvar eemacs-lspa/project-use-specific-architecture nil)
(defvar eemacs-lspa/project-use-specific-platform nil)
(defun eemacs-lspa/project-catch-env-var (type)
  (cl-case type
    (force-use-archive-p
     (or (string= (getenv "Eemacs_Lspa_Use_Archive") "t")
         eemacs-lspa/project-force-use-archive))
    (use-arch
     (or (getenv "Eemacs_Lspa_Use_Architecture")
         eemacs-lspa/project-use-specific-architecture))
    (use-platform
     (or (ignore-errors (intern (getenv "Eemacs_Lspa_Use_Platform")))
         eemacs-lspa/project-use-specific-platform
         system-type))
    (t
     (error "Unsupport type '%s'" type))))

;; *** main

(defun eemacs-lspa/project-get-current-system-architecture (system-platform)
  (or (eemacs-lspa/project-catch-env-var 'use-arch)
      (let ((cur-platform system-platform))
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
                   (shell-command-to-string "uname -m"))))))))

(defvar eemacs-lspa/project--current-make-prefix nil)
(defun eemacs-lspa/project-echo-make-prefix ()
  (let ((info eemacs-lspa/project--current-make-prefix))
    (message "")
    (message "Use-platform:     %s" (plist-get info :cur-platform))
    (message "Use-architecture: %s" (plist-get info :cur-architecture))
    (message "Use-archive-type: %s" (plist-get info :cur-archive-use-type))
    (message "")))

(defun eemacs-lspa/project-query-register (lspa-register)
  "return the loader or nil for unsupport various"
  (let* ((name (car lspa-register))
         (register-tree (cdr lspa-register))
         (register-root (expand-file-name (plist-get register-tree :root)
                                          eemacs-lspa/path-lspa-repos-root))
         (prebuilt-obj (plist-get register-tree :prebuilt))
         (archive-obj (plist-get register-tree :archive))
         (cur-platform (eemacs-lspa/project-catch-env-var 'use-platform))
         (cur-arch
          (eemacs-lspa/project-get-current-system-architecture cur-platform)))

    (setq eemacs-lspa/project--current-make-prefix
          (plist-put eemacs-lspa/project--current-make-prefix
                     :cur-platform cur-platform))
    (setq eemacs-lspa/project--current-make-prefix
          (plist-put eemacs-lspa/project--current-make-prefix
                     :cur-architecture cur-arch))

    (let* (builtin-support
           archive-support
           (reg-prebuilt-all (alist-get 'all prebuilt-obj))
           (reg-prebuilt-platform (alist-get cur-platform prebuilt-obj))
           (reg-prebuilt-platform-arch (alist-get cur-arch reg-prebuilt-platform))
           reg-prebuilt-use-all
           (reg-archive-all (alist-get 'all archive-obj))
           (reg-archive-platform (alist-get cur-platform archive-obj))
           (reg-archive-platform-arch (alist-get cur-arch reg-archive-platform))
           reg-archive-use-all)

      (catch :exit
        (unless (or (and reg-prebuilt-platform reg-prebuilt-platform-arch)
                    (and reg-prebuilt-all
                         (setq reg-prebuilt-use-all t)))
          (throw :exit nil))
        (if reg-prebuilt-use-all
            (setq builtin-support
                  (eemacs-lspa/project-expand-loader-for-prebuilt-all
                   register-root (plist-get reg-prebuilt-all :init)))
          (setq builtin-support (eemacs-lspa/project-expand-loader-for-prebuilt-individual
                                 register-root cur-platform cur-arch
                                 (plist-get reg-prebuilt-platform-arch :init)))))
      (catch :exit
        (unless (or (and reg-archive-platform reg-archive-platform-arch)
                    (and reg-archive-all
                         (setq reg-archive-use-all t)))
          (throw :exit nil))
        (if reg-archive-use-all
            (setq archive-support
                  (eemacs-lspa/project-expand-loader-for-archive-all
                   register-root (plist-get reg-archive-all :init)))
          (setq archive-support (eemacs-lspa/project-expand-loader-for-archive-individual
                                 register-root cur-platform cur-arch
                                 (plist-get reg-archive-platform-arch :init)))))
      (cond ((eemacs-lspa/project-catch-env-var 'force-use-archive-p)
             (setq eemacs-lspa/project--current-make-prefix
                   (plist-put eemacs-lspa/project--current-make-prefix
                              :cur-archive-use-type "Archive"))
             archive-support)
            (builtin-support
             (setq eemacs-lspa/project--current-make-prefix
                   (plist-put eemacs-lspa/project--current-make-prefix
                              :cur-archive-use-type "Prebuilt"))
             builtin-support)
            (archive-support
             (setq eemacs-lspa/project--current-make-prefix
                   (plist-put eemacs-lspa/project--current-make-prefix
                              :cur-archive-use-type "Archive"))
             archive-support)))))


;; * provide
(defvar eemacs-lspa/project-loaders-obtained nil)
(defvar eemacs-lspa/project-loaders-missing nil)

(let (rtn)
  (dolist (item eemacs-lspa/project-lspa-summary)
    (push (eemacs-lspa/project-query-register item) rtn))
  (setq eemacs-lspa/project-loaders-obtained rtn)
  (dolist (loader rtn)
    (when (and (not (ignore-errors (file-exists-p loader)))
               (not (null loader)))
      (add-to-list 'eemacs-lspa/project-loaders-missing loader)))
  (eemacs-lspa/project-echo-make-prefix)
  (when eemacs-lspa/project-loaders-missing
    (message
     "eemacs lspa registed loader missing,
please check variable `eemacs-lspa/project-loaders-missing'
")
    (dolist (el eemacs-lspa/project-loaders-missing)
      (message (format "missing file: %s" el)))
    (error "Mission corrupt!"))
  (cond ((not (null eemacs-lspa/project-loaders-obtained))
         (dolist (loader eemacs-lspa/project-loaders-obtained)
           (load loader)))
        ((null eemacs-lspa/project-loaders-obtained)
         (message "There's no available lspa for current architecture '%s'"
                  system-type))))

(provide 'eemacs-lsp-archive-load)
