;;; eemacs-lspa-subr.el --- The core subroutines for eemacs lsp archive project
;;
;; * Copyright (C) 20200413  entropy
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

;; The subroutines for entropy emacs lsp archive.

;; Source code was written with outline context wrapper, please read
;; the commentrary of each section for catching the mechanism and
;; viewing the APIs provision.

;; * Configuration:

;; Just load commonly.

;; * code
;; ** requirements
(require 'cl-lib)

;; ** customized variable
(defvar eemacs-lspa/subr-loader-indicator nil
  "When non-nil for forcely indicating using loading branch of
the project init procedure.")

;; ** internal variable declaration
(defvar eemacs-lspa/subr--print-count 1)

(defvar eemacs-lspa/subr--root-dir
  (file-name-directory
   (expand-file-name load-file-name)))

(defvar eemacs-lspa/subr--elisp-fmt-py-bin
  (expand-file-name "elisp-autofmt.py"
                    eemacs-lspa/subr--root-dir))

(defvar eemacs-lspa/subr--arch-alias
  '(("x64-based" x86_64)
    ("x86_64" x86_64)))

;; ** Api
;; *** Commonly API
;; **** Dir and file operation
;; ***** list directory
(defun eemacs-lspa/subr-list-dir-lite (dir-root)
  "Return directory list with type of whichever file or
directory."
  (let (rtn-full rtn-lite rtn-attr)
    (when (and (file-exists-p dir-root)
               (file-directory-p dir-root))
      (setq rtn-full (directory-files dir-root t))
      (dolist (el rtn-full)
        (if (not (string-match-p "\\(\\.$\\|\\.\\.$\\)" el))
            (push el rtn-lite)))
      (if rtn-lite
          (progn
            (dolist (el rtn-lite)
              (if (file-directory-p el)
                  (push `("D" . ,el) rtn-attr)
                (push `("F" . ,el) rtn-attr)))
            rtn-attr)
        nil))))

(defun eemacs-lspa/subr-list-subdir (dir-root)
  "List subdir of root dir DIR-ROOT"
  (let ((dirlist (eemacs-lspa/subr-list-dir-lite dir-root))
        (rtn nil))
    (if dirlist
        (progn
          (dolist (el dirlist)
            (if (equal "D" (car el))
                (push (cdr el) rtn)))
          (if rtn
              rtn
            nil))
      nil)))

(defun eemacs-lspa/subr-list-subfiles (dir-root)
  "List files under DIR-ROOT"
  (let ((dirlist (eemacs-lspa/subr-list-dir-lite dir-root))
        (rtn nil))
    (if dirlist
        (progn
          (dolist (el dirlist)
            (when (equal "F" (car el))
              (push (cdr el) rtn)))
          (if rtn
              rtn
            nil))
      nil)))

(defun eemacs-lspa/subr-list-dir-recursively (top-dir)
  "List subdirs recursively and return a list whose each element
can be a dir name or a subdir-data, a list whose car was the top
dir name and the rest was a subdir-data."
  (let ((subdirs (eemacs-lspa/subr-list-subdir top-dir))
        rtn)
    (catch :exit
      (add-to-list 'rtn top-dir)
      (unless subdirs
        (throw :exit nil))
      (dolist (sub-dir subdirs)
        (add-to-list
         'rtn
         (eemacs-lspa/subr-list-dir-recursively sub-dir))))
    (reverse rtn)))

(defun eemacs-lspa/subr-list-dir-recursive-for-list (top-dir)
  "List of all the subdirs under current TOP-DIR, it recursively
represents. "
  (let ((dir-struct (eemacs-lspa/subr-list-dir-recursively top-dir))
        ext-func)
    (setq
     ext-func
     (lambda (node)
       (let (rtn)
         (catch :exit
           (setq rtn (list (car node)))
           (unless (cdr node)
             (throw :exit nil))
           (dolist (sub-node (cdr node))
             (setq
              rtn
              (append rtn (funcall ext-func sub-node)))))
         rtn)))
    (funcall ext-func dir-struct)))

(defun eemacs-lspa/subr-list-files-recursive-for-list (top-dir)
  "List of all files under TOP-DIR recursively."
  (let ((dir-list (eemacs-lspa/subr-list-dir-recursive-for-list top-dir))
        rtn)
    (dolist (dir dir-list)
      (setq rtn (append rtn (eemacs-lspa/subr-list-subfiles dir))))
    rtn))

;; ***** file name parse
(defun eemacs-lspa/subr-file-path-parser (file-name type)
  "The file-path for 'entropy-emacs, functions for get base-name,
shrink trail slash, and return the parent(up level) dir.

type:

- 'non-trail-slash':

  Shrink the FILE-NAME path trail slash and return it.

- 'file-name':

  Return the file base name include its suffix type.

- 'parent-dir':

  Return its parent directory path using `file-name-directory'"
  (let (rtn (fname (replace-regexp-in-string "\\(\\\\\\|/\\)$" "" file-name)))
    (cl-case type
      ('non-trail-slash
       (setq rtn fname))
      ('file-name
       (setq rtn
             (replace-regexp-in-string
              "^.*\\(\\\\\\|/\\)\\([^ /\\\\]+\\)$"
              "\\2"
              fname)))
      ('parent-dir
       (setq rtn (file-name-directory fname))))
    rtn))

;; **** Add lsp server bins to emacs env

(defun eemacs-lspa/subr-add-path
    (shell-reg-path shell-reg-append exec-reg-path exec-reg-append)
  "Add SHELL-REG-PATH to ENV PATH and EXEC-REG-PATH to `exec-path'.

If SHELL-REG-APPEND non-nil appending the SHELL-REGE-PATH to the
ENV PATH tail. If EXEC-REG-APPEND non-nil, appending
EXEC-GET-PATH to the tail of `exec-path'."
  (let ((comma-style (if (eq (eemacs-lspa/subr-get-system-type) 'windows-nt) ";" ":")))
    (if shell-reg-append
        (setenv "PATH"
                (concat (getenv "PATH")
                        comma-style
                        shell-reg-path))
      (setenv "PATH"
              (concat shell-reg-path
                      comma-style
                      (getenv "PATH"))))
    (if exec-reg-append
        (setq exec-path
              (append exec-path
                      `(,exec-reg-path)))
      (add-to-list 'exec-path exec-reg-path))))

;; **** Interactive session judging
(defun eemacs-lspa/subr-noninteractive ()
  "Supress non-interactive emacs session `noninteractive' status
accroding to `eemacs-lspa/subr-loader-indicator' when non-nil,
otherwise return the value depent on `noninteractive'."
  (if eemacs-lspa/subr-loader-indicator
      nil
    noninteractive))

;; **** Read recipes

(defun eemacs-lspa/subr-read-recipes (recipes-root)
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

;; **** Echo make prefix prompt
(defvar eemacs-lspa/subr-current-make-prefix nil)
(defun eemacs-lspa/subr-echo-make-prefix (stdout)
  (let ((info eemacs-lspa/subr-current-make-prefix)
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

;; *** Folder structer node naming API
;; **** Folder name conventions
(defvar eemacs-lspa/subr-architecture-folder-alist
  '((x86_64 "x86_64")
    (aarch64 "aarch64"))
  "Architecture folder name reflects.")

(defvar eemacs-lspa/subr-platform-folder-alist
  '((gnu/linux "GnuLinux")
    (gnu "GnuHurd")
    (gnu/kfreebsd "GnuKfreebsd")
    (darwin "Darwin")
    (windows-nt "WindowsNT")
    (cygwin "Cygwin")
    (all "All"))
  "Platform folder name reflects.")

;; **** Get archive folder name

(defun eemacs-lspa/subr-get-platform-folder-name (system-platform)
  (let ((folder-name
         (car
          (alist-get
           system-platform
           eemacs-lspa/subr-platform-folder-alist))))
    (unless folder-name
      (error "Can not find platform folder name for platform '%s'"
             system-platform))
    folder-name))

(defun eemacs-lspa/subr-get-architecture-folder-name (system-architecture)
  (let ((folder-name
         (car
          (alist-get
           system-architecture
           eemacs-lspa/subr-architecture-folder-alist))))
    (unless folder-name
      (error "Can not find architecture folder name for architecture '%s'"
             system-architecture))
    folder-name))

;; *** System type get API
;; **** Optional env condition detected

(defun eemacs-lspa/subr-catch-env-var (type)
  (let ((detect-func (lambda (x) (if (string= x "") nil x))))
    (cl-case type
      (force-use-archive-p
       (string= (getenv "Eemacs_Lspa_Use_Archive") "t"))
      (use-arch
       (funcall detect-func (getenv "Eemacs_Lspa_Use_Architecture")))
      (use-platform
       (funcall detect-func (getenv "Eemacs_Lspa_Use_Platform")))
      (t
       (error "Unsupport type '%s'" type)))))


;; **** System type wrapper
(defun eemacs-lspa/subr-get-system-type ()
  (let ((rtn
         (or (ignore-errors (intern (eemacs-lspa/subr-catch-env-var 'use-platform)))
             system-type)))
    (unless (member
             rtn
             '(gnu
               gnu/linux
               gnu/kfreebsd
               darwin
               ms-dos
               windows-nt
               cygwin))
      (error "Unrecognize system platform type '%s'" rtn))
    rtn))

;; **** Get system architecture

(defun eemacs-lspa/subr-judge-architecture-validp (system-arch)
  (unless (member system-arch '(x86_64 aarch64))
    (error "Unknow system architecture '%s'!" system-arch)))

(defun eemacs-lspa/subr-get-current-system-architecture ()
  (let ((system-platform (eemacs-lspa/subr-get-system-type))
        architecture)
    (setq architecture
          (let ((cur-platform system-platform))
            (cond
             ((eq cur-platform 'windows-nt)
              (let ((sysinfo))
                (setq sysinfo
                      (car (split-string
                            (nth 1
                                 (split-string
                                  (shell-command-to-string
                                   "systeminfo | findstr /R \"System.Type\"")
                                  ":" t))
                            " " t)))
                (car (alist-get sysinfo eemacs-lspa/subr--arch-alias nil nil 'string=))))
             (t
              (intern (replace-regexp-in-string
                       "\n" ""
                       (shell-command-to-string "uname -m")))))))
    (eemacs-lspa/subr-judge-architecture-validp architecture)
    architecture))

;; *** Make and Load procedure wrapper
;; **** Procedure calling wrapper

(cl-defmacro eemacs-lspa/subr-common-do-with-prompt
    (message-make
     message-load
     &key ((:make-body make-body)) ((:load-body load-body))
     &allow-other-keys)
  "Do make body and load body with specific head prompts.

Ensure that each body must be include in a parentheses."
  `(let ()
     (if (eemacs-lspa/subr-noninteractive)
         (progn
           (message "")
           (message
            (format "==================================================\n[%s]: %s ..."
                    eemacs-lspa/subr--print-count
                    ,message-make))
           ,@make-body
           (message ""))
       (message "")
       (message
        (format "==================================================\n[%s]: %s ..."
                eemacs-lspa/subr--print-count
                ,message-load))
       ,@load-body
       (message ""))
     (message "Section task done!\n==================================================\n")
     (cl-incf eemacs-lspa/subr--print-count)))

;; **** Add shell batch

(defvar eemacs-lspa/subr-shell-batch-file
  (expand-file-name (format "eemacs-lspa-install.%s"
                            (if (eq (eemacs-lspa/subr-get-system-type) 'windows-nt) "cmd" "sh"))
                    eemacs-lspa/subr--root-dir)
  "Shell script file for bootstraping, for posix system using
bash script, windows use cmd batch, do not support any other script
language.")

(defun eemacs-lspa/subr-add-batch-file (item-prompt cmd)
  "Add shell script into `eemacs-lspa/subr-shell-batch-file'."
  (with-current-buffer
      (find-file-noselect eemacs-lspa/subr-shell-batch-file nil t)
    (let* ((inhibit-read-only t)
           (head-fmstr
            "
echo
echo ==============================
echo [%s]: %s
echo ==============================
"
            ))
      (when (eq (eemacs-lspa/subr-get-system-type) 'windows-nt)
        (goto-char (point-min))
        (let ((echo-off-not-inserted nil))
          (save-excursion
            (unless (re-search-forward "^@echo off" nil t)
              (setq echo-off-not-inserted t)))
          (when echo-off-not-inserted
            (insert "@echo off\n"))))
      (goto-char (point-max))
      (newline)
      (insert
       (format head-fmstr "Bootstrap"
               item-prompt))
      (newline)
      (insert cmd)
      (newline))
    (save-buffer)
    (kill-buffer)))

;; **** Lsp callers refactory
;; ***** pypi
;; ****** patch python installed bins for import portable "sys.path"
(defun eemacs-lspa/subr-pypi-patch-python-bin-for-pythonpath (pyfile site-packages-path)
  "Patch python file PYFILE for add =sys.path= for SITE-PACKAGES-PATH"
  (unless (file-exists-p pyfile)
    (error "Pyfile '%s' not existed!" pyfile))
  (with-current-buffer (find-file-noselect pyfile)
    (let ((inhibit-read-only t)
          (inst-str (format "sys.path.insert(0,\"%s\")"
                            (expand-file-name site-packages-path)))
          (inst-entry-regexp "^import sys")
          (inst-new nil))
      (goto-char (point-min))
      (unless (re-search-forward inst-entry-regexp nil t)
        (setq inst-new t))
      (if inst-new
          (progn
            (goto-char (point-min))
            (insert "import sys\n\n")
            (insert (concat inst-str "\n\n")))
        (goto-char (line-end-position))
        (newline)
        (insert inst-str))
      (save-buffer)
      (kill-buffer))))

;; ****** generate w32 python installed bins cmd wrapper
(defun eemacs-lspa/subr-pypi-gen-python-w32-cmd-bin (cmd-bin-file bin-file-abspath site-packages-abspath)
  "Generate windows cmd batch CMD-BIN-FILE for python callers in
windows platform and using library path SITE-PAKCAGE-ABSPATH."
  (let ((template
         "@ECHO off
CALL :find_dp0

SETLOCAL

SET \"PYTHONPATH=%s;%%PYTHONPATH%%\"
cd %s
.\\%s %%*

ENDLOCAL
EXIT /b %%errorlevel%%
:find_dp0
SET dp0=%%~dp0
EXIT /b
"
         ))

    (with-current-buffer (find-file-noselect cmd-bin-file)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (goto-char (point-min))
        (insert
         (format template
                 site-packages-abspath
                 (file-name-directory bin-file-abspath)
                 (file-name-base bin-file-abspath)))
        (save-buffer)
        (kill-buffer)))))

;; *** Prettify elisp file

(defun eemacs-lspa/subr-prettify-elisp-file (elisp-file)
  "Prettify elisp file."
  (message "Prettify elisp file '%s' ..." elisp-file)
  (shell-command
   (format "python %s %s"
           eemacs-lspa/subr--elisp-fmt-py-bin
           elisp-file))
  (message "Prettify elisp file '%s' done!" elisp-file))


;; * provide
(provide 'eemacs-lspa-subr)
