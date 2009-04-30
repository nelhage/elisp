;; Proto major-mode for emacs
;; Copyright (C) 2005-2008, Jonathan Bachrach, Jacob Beal, and contributors 
;; listed in the AUTHORS file in the MIT Proto distribution's top directory.
;;
;; This file is part of MIT Proto, and is distributed under the terms of
;; the GNU General Public License, with a linking exception, as described
;; in the file LICENSE in the MIT Proto distribution's top directory. */

;; This file lets emacs properly indent and color Proto code
;;
;; To use this major-mode, add the following line to your .emacs file:
;; (load "[PATH TO PROTO REPOSITORY]/man/proto-mode")
;;
;; Current needs: 
;; - indentation to do two-level let structures
;; - the run-code call

(defvar proto-mode-hook nil) ; place for user code to be added

(defvar proto-mode-map ;; make the key mappings
  (let ((proto-mode-map (make-keymap))
	(cmap (make-sparse-keymap)))
    (define-key proto-mode-map "\C-c\C-e" 'run-sim)
    proto-mode-map))

;; Auto-start the mode
(add-to-list 'auto-mode-alist '("\\.proto\\'" . proto-mode))

(defconst proto-font-lock-keywords
  (list
   ;; space-time keywords
   (cons (concat "\\b"
                 (regexp-opt '("if" "rep" "letfed" "once" "dt" 
                               "min-hood" "max-hood" "int-hood"
                               "any-hood" "all-hood"
                               "nbr" "nbr-range" "nbr-bearing"
                               ) t)
                 "\\b") 
         'font-lock-builtin-face)
   ;; deprecated, abstraction-violating, and other dangerous keywords
   (cons (concat "\\b"
                 (regexp-opt '("fold-hood" "sum-hood" "probe") t)
                 "\\b")
         'font-lock-warning-face)
   ;; other keywords
   (cons (concat "\\b"
                 (regexp-opt '("def" "let" "let*" "all" "seq" "mux" "and" "or"
                               "unless" "when" "cond" "case" "case-by" "fun" "#t" "#f"
                               "tup" "vec") t)
                 "\\b")
         'font-lock-keyword-face)))

;; Line indentation
(defun proto-indent-line ()
  (interactive)
  (beginning-of-line)
  (let ((cur-indent 0)
	(extra-indents (regexp-opt '("def" "if" "mux" "unless" "when") t))
	(let-indents (regexp-opt '("let" "let*" "letfed") t))) 
    (unless (bobp) ; first line indents to zero
      (condition-case nil
	  (save-excursion
	    (up-list -1) (forward-char) 
	    (setq cur-indent 
		  (+ (current-column)
		     (cond ((or (looking-at extra-indents) 
				(looking-at let-indents))
			    1)
			   (t 0)))))
	(error nil))
      (indent-line-to (max 0 cur-indent)))))
;; (lisp-indent-line) can be substituted as a fall-back behavior

;; syntax table stolen from Allegro CommonLISP
(defvar proto-mode-syntax-table nil)
(if (not proto-mode-syntax-table)
  (let ((i 0))
    (setq proto-mode-syntax-table (make-syntax-table))
    (while (< i ?0)
      (modify-syntax-entry i "_   " proto-mode-syntax-table)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " proto-mode-syntax-table)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " proto-mode-syntax-table)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " proto-mode-syntax-table)
      (setq i (1+ i)))
    (modify-syntax-entry ?  "    " proto-mode-syntax-table)
    (modify-syntax-entry ?\t "    " proto-mode-syntax-table)
    (modify-syntax-entry ?\n ">   " proto-mode-syntax-table)
    (modify-syntax-entry ?\f ">   " proto-mode-syntax-table)
    (modify-syntax-entry ?\; "<   " proto-mode-syntax-table)
    (modify-syntax-entry ?` "'   " proto-mode-syntax-table)
    (modify-syntax-entry ?' "'   " proto-mode-syntax-table)
    (modify-syntax-entry ?, "'   " proto-mode-syntax-table)
    (modify-syntax-entry ?. "'   " proto-mode-syntax-table)
    (modify-syntax-entry ?# "'   " proto-mode-syntax-table)
    (modify-syntax-entry ?\" "\"    " proto-mode-syntax-table)
    (modify-syntax-entry ?\\ "\\   " proto-mode-syntax-table)
    (modify-syntax-entry ?\( "()  " proto-mode-syntax-table)
    (modify-syntax-entry ?\) ")(  " proto-mode-syntax-table)
    (modify-syntax-entry ?\[ "(]  " proto-mode-syntax-table)
    (modify-syntax-entry ?\] ")[  " proto-mode-syntax-table)
    (modify-syntax-entry ?*   "w   " proto-mode-syntax-table)
    ;; The next syntax entry doesn't work with these forms:
    ;;  `,.foo
    ;;  #.foo
    ;; but it works better with variables with .'s in them
    (modify-syntax-entry ?. "w   " proto-mode-syntax-table)
    ;;(modify-syntax-entry ?\| "\"   " proto-mode-syntax-table)
    (modify-syntax-entry ?\[ "_   " proto-mode-syntax-table)
    (modify-syntax-entry ?\] "_   " proto-mode-syntax-table)))

(defun proto-mode ()
  "Major mode for editing Proto spatial computing language files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table proto-mode-syntax-table)
  (use-local-map proto-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(proto-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'proto-indent-line)
  (setq major-mode 'proto-mode)
  (setq mode-name "Proto")
  (run-hooks 'proto-mode-hook))

(provide 'proto-mode)


