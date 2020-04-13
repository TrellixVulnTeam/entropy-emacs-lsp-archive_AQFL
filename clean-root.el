(defvar $call-root
  (file-name-directory
   (expand-file-name load-file-name)))

(unless (executable-find "git")
  (error "You should install 'git' in your PATH firstly!"))

(let* ((default-directory $call-root)
       (output
        (shell-command-to-string
         "git status"))
       (not-init-p
        (string-match-p "fatal: not a git repository" output)))
  (when not-init-p
    (message (shell-command-to-string "git init"))
    (message (shell-command "git add .")))
  (message
   (shell-command-to-string
    "git clean -xfd .")))

(provide 'clean-root)
