;;; eemacs-lsp-archive-load.el --- eemacs lsp archive project
;;
;; * Copyright (C) 2020 Entropy(bmsac0001@gmail.com)
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
;; :PROPERTIES:
;; :CUSTOM_ID: h-b5013db2-37a9-44de-9327-05b17e760dbc
;; :END:

;; This project archived all [[https://github.com/c0001/entropy-emacs.git][entropy-emacs]] required Microsoft Language
;; Server implementations, and aimed for build and load all of them for
;; current working platform.

;; *Make:*

;; A simple make file stored in the root of this project, run it with
;; option =help= and =all= for quickly build for your machine
;; architecture, =aarch64=, =x86_64= are only supported ones, further
;; more, only operation system included in =Windows= and =gnu/linux= were
;; supported as so.

;; #+begin_quote
;; *Notice for make on windows platform:*

;; If you using Cygwin like posix emulator to make this project,
;; please confirm that all the kits used by this project's required
;; are all built by its toolchain, include =python=, =emacs=,
;; =make=, so that we can guarantee the 'make' environment keep
;; consistency.

;; Or you should using purely windows individual gnumake to build as
;; well. You can get windows version of 'gnumake' by any populate
;; windows packages manager like [[https://chocolatey.org/][chocolate]], and ensure that the
;; 'make' version equal or larger than 4.3.
;; #+end_quote

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

;; *Environment variables:*

;; We made the make procedure can be specified for particular
;; platform and architecture even for the archive using type, for
;; testing or other special meanings. The following three environment
;; variables were builtin for the sake of thus:

;; 1) =Eemacs_Lspa_Use_Archive=:

;;    The environment variable for indicate whether focely using
;;    archive type.

;;    Valid value are: 't' or 'nil'

;; 2) =Eemacs_Lspa_Use_Platform=:

;;    The environment variable for indicating which system platform to
;;    use, valid values are all of them can be getted by emacs
;;    internal variable =system-type=.

;; 3) =Eemacs_Lspa_Use_Architecture=

;;    The environment variable for indicating which architecture to
;;    use, valid values are 'x86_64' and 'aarch64'.

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
;; architecture sub-folders under those second hierarchy folder, see
;; the section [[#h-cc2e18fd-c581-4861-a1d2-5ee3a26d63c7][Get API]].

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
;; below sections. ([[#h-8960a582-196b-44d4-ad49-bbf74cc943d6][recipe]])

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
;; (python-language-server
;;  :root
;;  "python-language-server"
;;  :prebuilt
;;  ((gnu/linux
;;    ((x86_64 :init "eemacs-lspa-pyls-prebuilt-gnulinux-x86_64-load.el")
;;     (aarch64 :init "eemacs-lspa-pyls-prebuilt-gnulinux-aarch64-load.el")))
;;   (windows-nt
;;    ((x86_64 :init "eemacs-lspa-pyls-prebuilt-windowsnt-x86_64-load.el"))))
;;  :archive nil)
;; #+end_src

;; As see the sample, we use the abbreviation path for the archvie
;; root, which will be automatically expanding with
;; =eemacs-lspa/path-lspa-repos-root=.

;; *Create recipe using template:*

;; You can create a recipe template via using ~make create~ by the
;; project makefile =create= rule, it create recipe for every
;; architecture of every platform for be as a form, you should
;; pruning the extra branches that you don't care about. And the
;; created template is stored in the place as well as the convention
;; of this project, so that you can directly edit it without any
;; targets movement.

;; *** Get APIs
;; :PROPERTIES:
;; :CUSTOM_ID: h-cc2e18fd-c581-4861-a1d2-5ee3a26d63c7
;; :END:

;; To all the contributors for writting their own recipes, we
;; recommend to use the project built-in APIs for keeping consistency
;; and obeying the project conventions. All the APIs are hosted on
;; =elements/library/*.el=, read their commentrary for details.

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

;; ** Register parse
;; *** library
;; **** Loader expand
;; ***** Prebuilt
(defun eemacs-lspa/project-expand-loader-for-prebuilt-individual (recipe-host-root platform arch loader-name)
  (let ((pltfname (eemacs-lspa/subr-get-platform-folder-name platform))
        (archfname (eemacs-lspa/subr-get-architecture-folder-name arch)))
    (expand-file-name
     loader-name
     (expand-file-name
      archfname
      (expand-file-name
       pltfname
       (expand-file-name "PreBuilt" (expand-file-name recipe-host-root)))))))

(defun eemacs-lspa/project-expand-loader-for-prebuilt-all (recipe-host-root loader-name)
  (expand-file-name
   loader-name
   (expand-file-name
    (eemacs-lspa/subr-get-platform-folder-name 'all)
    (expand-file-name
     "PreBuilt" (expand-file-name recipe-host-root)))))

;; ***** Archive
(defun eemacs-lspa/project-expand-loader-for-archive-individual (recipe-host-root platform arch loader-name)
  (let ((pltfname (eemacs-lspa/subr-get-platform-folder-name platform))
        (archfname (eemacs-lspa/subr-get-architecture-folder-name arch)))
    (expand-file-name
     loader-name
     (expand-file-name
      archfname
      (expand-file-name
       pltfname
       (expand-file-name "Archive" (expand-file-name recipe-host-root)))))))

(defun eemacs-lspa/project-expand-loader-for-archive-all (recipe-host-root loader-name)
  (expand-file-name
   loader-name
   (expand-file-name
    (eemacs-lspa/subr-get-platform-folder-name 'all)
    (expand-file-name
     "Archive" (expand-file-name recipe-host-root)))))

;; **** Echo make prefix prompt
(defvar eemacs-lspa/project-current-make-prefix nil)
(defun eemacs-lspa/project-echo-make-prefix (stdout)
  (let ((info eemacs-lspa/project-current-make-prefix)
        (inject-func
         (lambda (fmt &rest args)
           (let ((str (apply 'format fmt args)))
             (message str)
             (insert (concat str "\n"))))))
    (dolist (item info)
      (with-current-buffer (find-file-noselect stdout)
        (let ((inhibit-read-only t))
          (funcall inject-func "")
          (funcall inject-func "==================================================")
          (funcall inject-func "Use-recipe:       %s" (plist-get item :recipe-name))
          (funcall inject-func "Use-platform:     %s" (plist-get item :for-platform))
          (funcall inject-func "Use-architecture: %s" (plist-get item :for-architecture))
          (funcall inject-func "Use-archive-type: %s" (plist-get item :archive-use-type))
          (funcall inject-func "==================================================")
          (funcall inject-func ""))
        (save-buffer)))))

;; **** Read recipes

(defun eemacs-lspa/project-read-recipes (recipes-root)
  (let ((recipes-dir (eemacs-lspa/subr-list-dir-lite recipes-root))
        recipes
        rtn)
    (dolist (node recipes-dir)
      (when (equal "F" (car node))
        (push (cdr node) recipes)))
    (when recipes
      (dolist (rcp recipes)
        (let ((buff (find-file-noselect rcp)))
          (with-current-buffer buff
            (goto-char (point-min))
            (push (read buff) rtn)))))
    rtn))

;; *** main

(defun eemacs-lspa/project-query-register (lspa-register)
  "return the loader or nil for unsupport various"
  (let* ((name (car lspa-register))
         (register-tree (cdr lspa-register))
         (register-root (expand-file-name (plist-get register-tree :root)
                                          eemacs-lspa/path-lspa-repos-root))
         (prebuilt-obj (plist-get register-tree :prebuilt))
         (archive-obj (plist-get register-tree :archive))
         (cur-platform (eemacs-lspa/subr-get-system-type))
         (cur-arch
          (eemacs-lspa/subr-get-current-system-architecture))
         (cur-make-prompt
          (list :recipe-name name
                :for-platform cur-platform
                :for-architecture cur-arch))
         rtn)

    (let* (builtin-support
           archive-support
           (reg-prebuilt-all (alist-get 'all prebuilt-obj))
           (reg-prebuilt-platform (car (alist-get cur-platform prebuilt-obj)))
           (reg-prebuilt-platform-arch (alist-get cur-arch reg-prebuilt-platform))
           reg-prebuilt-use-all
           (reg-archive-all (alist-get 'all archive-obj))
           (reg-archive-platform (car (alist-get cur-platform archive-obj)))
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
      (setq rtn
            (cond ((and (eemacs-lspa/subr-catch-env-var 'force-use-archive-p)
                        archive-support)
                   (setq cur-make-prompt
                         (plist-put cur-make-prompt
                                    :archive-use-type "Archive"))
                   archive-support)
                  (builtin-support
                   (setq cur-make-prompt
                         (plist-put cur-make-prompt
                                    :archive-use-type "Prebuilt"))
                   builtin-support)
                  (archive-support
                   (setq cur-make-prompt
                         (plist-put cur-make-prompt
                                    :archive-use-type "Archive"))
                   archive-support)
                  (t
                   (setq cur-make-prompt
                         (plist-put cur-make-prompt
                                    :archive-use-type "No match for current system"))))))
    (push cur-make-prompt eemacs-lspa/project-current-make-prefix)
    rtn))


;; * provide
(defvar eemacs-lspa/project-loaders-obtained nil)
(defvar eemacs-lspa/project-loaders-missing nil)
(defvar eemacs-lspa/project-lspa-summary
  (eemacs-lspa/project-read-recipes
   eemacs-lspa/path-lspa-recipes-root))
(defvar eemacs-lspa/project-log-file
  (expand-file-name "log.txt"
                    eemacs-lspa/path-project-root))

(let ((exec-requests '("pip" "npm"))
      (cnt 0))
  (dolist (exec exec-requests)
    (unless (executable-find exec)
      (progn
        (cl-incf cnt)
        (message "[%s]: missing exec '%s'" cnt exec))))
  (when (> cnt 0)
    (error "Please install above missing binaries. Abort!"))
  (when (version< (replace-regexp-in-string
                   "\n" ""
                   (replace-regexp-in-string
                    "^.+ \\(\\([0-9]\\.?\\)+[0-9]+\\).*"
                    "\\1"
                    (shell-command-to-string "python --version")))
                  "3.8")
    (error "You must install python version up to 3.8 ")))

(let (rtn)
  (dolist (item eemacs-lspa/project-lspa-summary)
    (push (eemacs-lspa/project-query-register item) rtn))
  (setq eemacs-lspa/project-loaders-obtained rtn)
  (dolist (loader rtn)
    (when (and (not (ignore-errors (file-exists-p loader)))
               (not (null loader)))
      (add-to-list 'eemacs-lspa/project-loaders-missing loader)))
  (eemacs-lspa/project-echo-make-prefix eemacs-lspa/project-log-file)
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
                  (eemacs-lspa/subr-get-system-type)))))

(provide 'eemacs-lsp-archive-load)
