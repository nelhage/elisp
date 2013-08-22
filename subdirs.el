(dolist (dir (directory-files "~/.elisp" t "^[^.]"))
  (when (file-directory-p dir)
    (setq load-path (cons dir load-path))))
