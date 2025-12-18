;;; realize.el --- Major mode for Realize IR files -*- lexical-binding: t; -*-

;;; Commentary:
;; A major mode for editing Realize IR (.rir) files.
;; Realize is a symbolic IR for tensor algebra.

;;; Code:

(defgroup realize nil
  "Major mode for Realize IR files."
  :group 'languages)

(defface realize-variable-face
  '((t :inherit font-lock-variable-name-face))
  "Face for variables (%name)."
  :group 'realize)

(defface realize-shape-face
  '((t :inherit font-lock-type-face))
  "Face for shapes/dimensions."
  :group 'realize)

(defface realize-operation-face
  '((t :inherit font-lock-function-name-face))
  "Face for operations."
  :group 'realize)

(defface realize-attribute-face
  '((t :inherit font-lock-constant-face))
  "Face for attributes (strides, sizes, etc)."
  :group 'realize)

(defconst realize-keywords
  '("function" "return")
  "Keywords in Realize IR.")

(defconst realize-operations
  '("scalar" "matmul" "select" "access" "write" "assume_positive" "constant"
    ;; binop variants
    "binop.add" "binop.mul" "binop.sub" "binop.div"
    "binop.max" "binop.min"
    "binop.gt" "binop.ge" "binop.lt" "binop.le" "binop.eq" "binop.ne"
    ;; unary variants
    "unary.neg" "unary.sqrt" "unary.exp" "unary.log"
    "unary.rsqrt" "unary.abs" "unary.reciprocal"
    ;; reduce variants
    "reduce.sum" "reduce.max" "reduce.min")
  "Operations in Realize IR.")

(defconst realize-attributes
  '("strides" "sizes" "offset" "axes" "shape" "dtype" "values")
  "Attribute names in Realize IR.")

(defvar realize-font-lock-keywords
  `(;; Comments
    ("#.*$" . font-lock-comment-face)
    ;; Keywords
    (,(regexp-opt realize-keywords 'symbols) . font-lock-keyword-face)
    ;; Variables (%name, %name.1, etc)
    ("%[a-zA-Z_][a-zA-Z0-9_.]*" . 'realize-variable-face)
    ;; Operations
    (,(regexp-opt realize-operations 'symbols) . 'realize-operation-face)
    ;; Attributes inside brackets
    (,(concat "\\<" (regexp-opt realize-attributes) "\\>") . 'realize-attribute-face)
    ;; Numbers (integers and floats, including scientific notation)
    ("-?[0-9]+\\(?:\\.[0-9]*\\)?\\(?:[eE][+-]?[0-9]+\\)?" . font-lock-constant-face)
    ;; Shape tuples - highlight the parentheses
    ("\\(([0-9, ]*)\\)" 1 'realize-shape-face))
  "Font lock keywords for Realize mode.")

(defvar realize-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments start with #
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; % is part of variable names
    (modify-syntax-entry ?% "_" table)
    ;; . is part of identifiers (for binop.add, %var.1, etc)
    (modify-syntax-entry ?. "_" table)
    table)
  "Syntax table for Realize mode.")

(defun realize-indent-line ()
  "Indent current line for Realize mode."
  (interactive)
  (let ((indent 0)
        (pos (- (point-max) (point))))
    (save-excursion
      (beginning-of-line)
      (cond
       ;; Closing brace - no indent
       ((looking-at "^[ \t]*}")
        (setq indent 0))
       ;; Inside function body
       ((save-excursion
          (and (re-search-backward "[{}]" nil t)
               (looking-at "{")))
        (setq indent 2))
       ;; Default
       (t (setq indent 0))))
    (indent-line-to indent)
    ;; Move point after indentation if it was before
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))))

;;;###autoload
(define-derived-mode realize-mode prog-mode "Realize"
  "Major mode for editing Realize IR files.

\\{realize-mode-map}"
  :syntax-table realize-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(realize-font-lock-keywords))
  (setq-local indent-line-function #'realize-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rir\\'" . realize-mode))

(provide 'realize)
;;; realize.el ends here
