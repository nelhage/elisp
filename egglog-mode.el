;;; egglog-mode.el --- Major mode for egglog files -*- lexical-binding: t; -*-

;; Author: Nelson Elhage
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages
;; URL: https://github.com/egraphs-good/egglog

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Major mode for editing egglog files (.egg).
;; Egglog is an equality saturation language combining e-graphs with Datalog.
;; This mode provides syntax highlighting for egglog keywords and builds
;; on Emacs' lisp/scheme mode for sexp-based editing.

;;; Code:

(require 'scheme)

;;; Customization ==============================================================

(defgroup egglog nil
  "Major mode for editing egglog files."
  :prefix "egglog-"
  :group 'languages
  :link '(url-link "https://github.com/egraphs-good/egglog"))

(defcustom egglog-indent-offset 2
  "Indentation offset for egglog-mode."
  :type 'integer
  :group 'egglog)

;;; Font Lock ==================================================================

(defconst egglog-keywords
  '(;; Data definition
    "datatype" "datatypes" "sort" "function" "relation" "constructor"
    ;; Rule definition
    "rule" "rewrite" "birewrite" "ruleset" "add-ruleset"
    "unstable-combined-ruleset"
    ;; Control flow / commands
    "let" "extract" "check" "run" "run-schedule" "fail"
    "include" "input" "output" "push" "pop"
    ;; Printing / debugging
    "print-function" "print-size" "print-stats" "print-overall-statistics"
    ;; Scheduling
    "saturate" "repeat" "sequence" "seq"
    ;; Experimental features (egglog-experimental)
    "for" "with-ruleset" "set-cost" "run-with" "get-size!")
  "Egglog language keywords.")

(defconst egglog-actions
  '("union" "set" "subsume" "delete" "panic" "extract" "let")
  "Egglog action keywords.")

(defconst egglog-builtins
  '(;; Boolean
    "true" "false" "and" "or" "not" "xor" "=>" "bool-="
    ;; Comparison / logic
    "=" "!=" "<" ">" "<=" ">="
    ;; Arithmetic
    "+" "-" "*" "/" "%" "^" "min" "max" "abs" "neg"
    "log" "sqrt" "pow" "floor" "ceil" "round"
    ;; Bitwise
    "<<" ">>" "&" "|" "bitor" "bitand" "bitnot" "bitxor"
    ;; String
    "to-string" "to-i64" "to-f64" "concat" "count-matches" "replace"
    ;; I64/F64/Rational primitives
    "i64" "f64" "rational" "string" "unit"
    ;; Container types
    "Vec" "Map" "Set"
    ;; Special
    "if" "switch" "cond")
  "Egglog built-in functions and types.")

(defconst egglog-attributes
  '(":cost" ":merge" ":on_merge" ":default" ":subsume"
    ":no-merge" ":mode" ":unextractable" ":name" ":ruleset"
    ":when" ":until" ":variants")
  "Egglog keyword attributes (colon-prefixed).")

(defconst egglog-font-lock-keywords
  (let ((keywords-regexp (regexp-opt egglog-keywords 'symbols))
        (actions-regexp (regexp-opt egglog-actions 'symbols))
        (builtins-regexp (regexp-opt egglog-builtins 'symbols))
        (attributes-regexp (regexp-opt egglog-attributes)))
    `(;; Keywords at start of sexp
      (,(concat "(" keywords-regexp) (1 font-lock-keyword-face))
      ;; Actions
      (,(concat "(" actions-regexp) (1 font-lock-builtin-face))
      ;; Built-in functions/types
      (,builtins-regexp . font-lock-builtin-face)
      ;; Attributes like :cost, :merge
      (,attributes-regexp . font-lock-constant-face)
      ;; Type/sort declarations (capitalized words after datatype/sort/function)
      ("\\<[A-Z][a-zA-Z0-9_-]*\\>" . font-lock-type-face)
      ;; Variables (words starting with ?)
      ("\\?[a-zA-Z_][a-zA-Z0-9_-]*" . font-lock-variable-name-face)
      ;; Numbers
      ("\\<-?[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)))
  "Font lock keywords for egglog-mode.")

;;; Syntax Table ===============================================================

(defvar egglog-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Inherit from scheme's syntax table for sexp handling
    (set-char-table-parent table scheme-mode-syntax-table)
    ;; Comments: ; starts a comment to end of line
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Parentheses
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    ;; Symbols can contain these
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?: "_" table)
    table)
  "Syntax table for egglog-mode.")

;;; Indentation ================================================================

(defun egglog-indent-line ()
  "Indent current line as egglog code."
  (interactive)
  (let ((indent (egglog-calculate-indent)))
    (when indent
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to indent))
      (when (< (current-column) indent)
        (back-to-indentation)))))

(defun egglog-calculate-indent ()
  "Calculate indentation for current line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let ((depth (car (syntax-ppss (point)))))
      (cond
       ;; Closing paren - align with opening paren
       ((looking-at ")")
        (forward-char 1)
        (backward-sexp 1)
        (current-column))
       ;; Otherwise indent based on paren depth
       (t (* depth egglog-indent-offset))))))

;;; Imenu ======================================================================

(defvar egglog-imenu-generic-expression
  '(("Datatypes" "^(datatype\\s-+\\([A-Za-z_][A-Za-z0-9_-]*\\)" 1)
    ("Sorts" "^(sort\\s-+\\([A-Za-z_][A-Za-z0-9_-]*\\)" 1)
    ("Functions" "^(function\\s-+\\([A-Za-z_][A-Za-z0-9_-]*\\)" 1)
    ("Relations" "^(relation\\s-+\\([A-Za-z_][A-Za-z0-9_-]*\\)" 1)
    ("Rulesets" "^(\\(?:add-\\)?ruleset\\s-+\\([A-Za-z_][A-Za-z0-9_-]*\\)" 1)
    ("Rules" "^(rule\\s-+" 0)
    ("Rewrites" "^(\\(?:bi\\)?rewrite\\s-+" 0))
  "Imenu generic expression for egglog-mode.")

;;; Major Mode =================================================================

;;;###autoload
(define-derived-mode egglog-mode prog-mode "Egglog"
  "Major mode for editing egglog files.

Egglog is an equality saturation language that combines e-graphs
with Datalog-style rules for program optimization and verification.

\\{egglog-mode-map}"
  :syntax-table egglog-mode-syntax-table
  :group 'egglog

  ;; Comments
  (setq-local comment-start "; ")
  (setq-local comment-start-skip ";+\\s-*")
  (setq-local comment-end "")
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)

  ;; Font lock
  (setq-local font-lock-defaults
              '(egglog-font-lock-keywords
                nil nil
                ((?- . "w") (?_ . "w") (?? . "w") (?! . "w"))))

  ;; Indentation
  (setq-local indent-line-function #'egglog-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width egglog-indent-offset)

  ;; Sexp navigation (inherit from lisp)
  (setq-local forward-sexp-function nil)
  (setq-local parse-sexp-ignore-comments t)

  ;; Imenu
  (setq-local imenu-generic-expression egglog-imenu-generic-expression)

  ;; Electric indent
  (setq-local electric-indent-chars '(?\) ?\n)))

;;; Keymap =====================================================================

(defvar egglog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    map)
  "Keymap for egglog-mode.")

;;; Auto-mode ==================================================================

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.egg\\'" . egglog-mode))

(provide 'egglog-mode)

;;; egglog-mode.el ends here
