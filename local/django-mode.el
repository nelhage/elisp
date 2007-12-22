; A (rather minimal) major mode for Emacs to edit Django templates
; by Antonio Cavedoni <http://cavedoni.com/>
;
; Hacked together following instructions from the rather awesome: 
; http://two-wugs.net/emacs/mode-tutorial.html

(defvar django-mode-hook nil)

(defvar django-mode-map
  (let ((django-mode-map (make-keymap)))
    (define-key django-mode-map "\C-j" 'newline-and-indent)
    django-mode-map)
  "Keymap for Django major mode")

(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . django-mode))

(defconst django-font-lock-keywords-1
  (list
   '("{% ?comment ?%}\\(\n?.*?\\)+?{% ?endcomment ?%}" . font-lock-comment-face)
   '("{% ?\\(\\(end\\)?\\(extends\\|for\\|cycle\\|filter\\|firstof\\|debug\\|if\\|ifchanged\\|ifequal\\|ifnotequal\\|include\\|load\\|now\\|regroup\\|spaceless\\|ssi\\|templatetag\\|widthratio\\|block\\)\\) ?.*? ?%}" . 1)
   '("{{ ?\\(.*?\\) ?}}" . (1 font-lock-variable-name-face))
   '("{%\\|\\%}\\|{{\\|}}" . font-lock-builtin-face)
   "Minimal highlighting expressions for Django mode"))

(defvar django-font-lock-keywords django-font-lock-keywords-1
  "Default highlighting expressions for Django mode")

(defvar django-mode-syntax-table
  (let ((django-mode-syntax-table (make-syntax-table)))
    django-mode-syntax-table)
  "Syntax table for django-mode")

(defun django-mode ()
  "Major mode for editing Django templates"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table django-mode-syntax-table)
  (use-local-map django-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(django-font-lock-keywords))
  (setq major-mode 'django-mode)
  (setq mode-name "Django")
  (run-hooks 'django-mode-hook))

(provide 'django-mode)
