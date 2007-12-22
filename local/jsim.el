(require 'compile)

(defvar jsim-mode-syntax-table nil
  "Syntax table for use in JSim-mode buffers.")

(if jsim-mode-syntax-table
    ()
  (setq jsim-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?/ ". 124b" jsim-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" jsim-mode-syntax-table)
  (modify-syntax-entry ?=  "."  jsim-mode-syntax-table)
  (modify-syntax-entry ?\n "> b"  jsim-mode-syntax-table)
  )

(define-derived-mode jsim-mode fundamental-mode "JSim"
  "Major mode for editing JSim netlists."

  (kill-all-local-variables)

  (set-syntax-table jsim-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "// ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/[/*]")
  (make-local-variable 'comment-end)
  (setq comment-end "")

  ;; Font-lock stuff
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults jsim-font-lock-defaults)
  )

(defvar jsim-font-lock-defaults
  `(jsim-font-lock-keywords
    nil		; do fontify strings and comments
    nil		; case is significant
    ,(mapcar (function (lambda (c) (cons c "w")))
	     "[]$_.:#") ; make these word syntax for font-lock
    nil
    ))

(defconst jsim-font-lock-keywords '(
   "^\\s-*\\.ac\\>"
   "^\\s-*\\.checkoff\\>"
   "^\\s-*\\.connect\\>"
   "^\\s-*\\.dc\\>"
   "^\\s-*\\.dsubckt\\>"
   "^\\s-*\\.ends\\>"
   "^\\s-*\\.global\\>"
   "^\\s-*\\.gsubckt\\>"
   "^\\s-*\\.include\\>"
   "^\\s-*\\.model\\>"
   "^\\s-*\\.op\\>"
   "^\\s-*\\.options\\>"
   "^\\s-*\\.plot\\>"
   "^\\s-*\\.plotdef\\>"
   "^\\s-*\\.subckt\\>"
   "^\\s-*\\.temp\\>"
   "^\\s-*\\.tempdir\\>"
   "^\\s-*\\.tran\\>"
   "^\\s-*\\.verify\\>"
   "^\\s-*\\+"
   ("^\\s-*\\*.*$" . font-lock-comment-face)
   ("\\(\\sw*=\\)\\sw*" 1 font-lock-builtin-face)
   ("^\\s-*\\.[gd]?subckt\\s-*\\(\\sw*\\)" 1 font-lock-function-name-face)
   )
  "Fontification for JSim code.")

(provide 'jsim)


