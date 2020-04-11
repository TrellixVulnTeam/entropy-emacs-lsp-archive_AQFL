;; * code
;; ** requirements
(require 'cl-lib)

(defvar eemacs-lspa/subr-loader-indicator nil)

(defvar eemacs-lspa/subr--print-count 1)

(defun eemacs-lspa/subr-noninteractive ()
  (if eemacs-lspa/subr-loader-indicator
      nil
    noninteractive))

(defvar eemacs-lspa/subr-root-dir
  (file-name-directory
   (expand-file-name load-file-name)))

(defvar eemacs-lspa/subr-shell-batch-file
  (expand-file-name (format "eemacs-lspa-install.%s"
                            (if (eq system-type 'windows-nt) "cmd" "sh"))
                    eemacs-lspa/subr-root-dir))

;; ** common library
;; *** dir and file operation
;; **** list directory
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
  (let ((dir-list (eemacs-lspa/subr-list-dir-recursive-for-list top-dir))
        rtn)
    (dolist (dir dir-list)
      (setq rtn (append rtn (eemacs-lspa/subr-list-subfiles dir))))
    rtn))

;; **** file name parse
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

;; *** path register

(defun eemacs-lspa/subr-add-path
    (shell-reg-path shell-reg-append exec-reg-path exec-reg-append)
  (let ((comma-style (if (eq system-type 'windows-nt) ";" ":")))
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

;; *** procedure message wrapper

(cl-defmacro eemacs-lspa/subr-common-do-with-prompt
    (message-make
     message-load
     &key ((:make-body make-body)) ((:load-body load-body))
     &allow-other-keys)
  `(let ()
     (if (eemacs-lspa/subr-noninteractive)
         (progn
           (message "")
           (message
            (format "[%s]: %s ..."
                    eemacs-lspa/subr--print-count
                    ,message-make))
           ,@make-body
           (message ""))
       (message "")
       (message
        (format "[%s]: %s ..."
                eemacs-lspa/subr--print-count
                ,message-load))
       ,@load-body
       (message ""))
     (cl-incf eemacs-lspa/subr--print-count)))

;; *** Add shell batch

(defun eemacs-lspa/subr-add-batch-file (item-prompt cmd)
  (with-current-buffer
      (find-file-noselect eemacs-lspa/subr-shell-batch-file)
    (let* ((inhibit-read-only t)
           (head-fmstr
            "
echo
echo ==============================
echo [%s]: %s
echo ==============================
"
            ))
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

;; ** lsp callers refactory
;; *** pypi
;; **** patch python installed bins for import portable "sys.path"
(defun eemacs-lspa/subr-pypi-patch-python-bin-for-pythonpath (pyfile site-packages-path)
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

;; **** generate w32 python installed bins cmd wrapper
(defun eemacs-lspa/subr-pypi-gen-python-w32-cmd-bin (cmd-bin-file bin-file-abspath site-packages-abspath)
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

;; ** read recipes
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

;; * provide
(provide 'eemacs-lspa-subr)
