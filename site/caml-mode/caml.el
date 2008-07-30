;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; caml.el - Caml mode for Emacs and XEmacs (20 and more).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Copyright © 1997-2000 Albert Cohen, all rights reserved.
;;         Copying is covered by the GNU General Public License.

;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.

;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;    GNU General Public License for more details.

(defconst caml-mode-version "Caml Mode"
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Emacs versions support

(defconst caml-with-xemacs (string-match "XEmacs" emacs-version))

(defconst caml-window-system
  (or (and (boundp 'window-system) window-system)
      (and (fboundp 'console-type) (or (eq (console-type) 'x)
				       (eq (console-type) 'win32))))
  "Are we running under a window system?")

(defun caml-match-string (num &optional string)
  "Return string of text matched by last search.

NUM specifies which parenthesized expression in the last regexp.
Value is nil if NUMth pair didn't match, or there were less than NUM
pairs.  Zero means the entire text matched by the whole regexp or
whole string.  STRING should be given if the last search was by
`string-match' on STRING."
  (let* ((data (match-data))
	 (begin (nth (* 2 num) data))
	 (end (nth (1+ (* 2 num)) data)))
    (if string (substring string begin end)
      (buffer-substring-no-properties begin end))))

(if (fboundp 'functionp) ()
  (defun functionp (obj)
    "Returns t if OBJ is a function, nil otherwise."
    (cond
     ((symbolp obj) (fboundp obj))
     ((subrp obj))
     ((compiled-function-p obj))
     ((consp obj)
      (if (eq (car obj) 'lambda) (listp (car (cdr obj)))))
     (t nil))))

(if (fboundp 'caar) ()
  (defun caar (l) (car (car l)))
  (defun cdar (l) (cdr (car l)))
  (defun cadr (l) (car (cdr l)))
  (defun cddr (l) (cdr (cdr l))))

(if (fboundp 'cadar) ()
  (defun cadar (l) (car (cdar l)))
  (defun cddar (l) (cdr (cdar l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       User customizable variables

;; use the standard `customize' interface or `caml-mode-hook' to
;; configure these variables

(require 'custom)

(defgroup caml nil
  "Support for the Objective Caml language."
  :group 'languages)

;; comments

(defcustom caml-indent-leading-comments t
  "*If true, indent leading comment lines (starting with `(*') like others."
  :group 'caml :type 'boolean)

(defcustom caml-indent-comments t
  "*If true, automatically align multi-line comments."
  :group 'caml :type 'boolean)

(defcustom caml-comment-end-extra-indent 0
  "*How many spaces to indent a leading comment end `*)'.
If you expect comments to be indented like
	(*
          ...
	 *)
even without leading `*', use `caml-comment-end-extra-indent' = 1."
  :group 'caml
  :type '(radio :extra-offset 8
		:format "%{Comment End Extra Indent%}:
   Comment alignment:\n%v"
		(const :tag "align with `(' in comment opening" 0)
		(const :tag "align with `*' in comment opening" 1)
		(integer :tag "custom alignment" 0)))

(defcustom caml-support-leading-star-comments t
  "*If true, allow automatic intentation of comments of the form
        (*
         * ...
         *)
If you still expect comments to be indented like
	(*
          ...
	 *)
without leading `*', use `caml-comment-end-extra-indent' = 1."
  :group 'caml :type 'boolean)

;; indentation defaults

(defcustom caml-default-indent 2
  "*Default indentation.

Global indentation variable (large values may lead to indentation overflows).
When no governing keyword is found, this value is used to indent the line
if it has to."
  :group 'caml :type 'integer)

(defcustom caml-lazy-= nil
  "*If true, indent `=' like a standard keyword (not `:=', `<='...)."
  :group 'caml :type 'boolean)

(defcustom caml-lazy-paren nil
  "*If true, indent parentheses like a standard keyword."
  :group 'caml :type 'boolean)

(defcustom caml-let-always-indent t
  "*If true, enforce indentation is at least `caml-let-indent' after a `let'.

As an example, set it to false when you have `caml-with-indent' set to 0,
and you want `let x = match ... with' and `match ... with' indent the
same way."
  :group 'caml :type 'boolean)

(defcustom caml-|-extra-unindent caml-default-indent
  "*Extra backward indent for Caml lines starting with the `|' operator.

It is automatically added to `function', `with', `parse' and some cases
of `type' keywords to leave enough space for `|' backward indentation."
  :group 'caml :type 'integer)

(defcustom caml-class-indent caml-default-indent
  "*How many spaces to indent from a `class' keyword."
  :group 'caml :type 'integer)

(defcustom caml-sig-struct-align t
  "*Align `sig' and `struct' keywords with `module'."
  :group 'caml :type 'boolean)

(defcustom caml-sig-struct-indent caml-default-indent
  "*How many spaces to indent from a `sig' or `struct' keyword."
  :group 'caml :type 'integer)

(defcustom caml-method-indent caml-default-indent
  "*How many spaces to indent from a `method' keyword."
  :group 'caml :type 'integer)

(defcustom caml-begin-indent caml-default-indent
  "*How many spaces to indent from a `begin' keyword."
  :group 'caml :type 'integer)

(defcustom caml-for-while-indent caml-default-indent
  "*How many spaces to indent from a `for' or `while' keyword."
  :group 'caml :type 'integer)

(defcustom caml-do-indent caml-default-indent
  "*How many spaces to indent from a `do' keyword."
  :group 'caml :type 'integer)

(defcustom caml-fun-indent caml-default-indent
  "*How many spaces to indent from a `fun' keyword."
  :group 'caml :type 'integer)

(defcustom caml-function-indent caml-default-indent
  "*How many spaces to indent from a `function' keyword."
  :group 'caml :type 'integer)

(defcustom caml-if-then-else-indent caml-default-indent
  "*How many spaces to indent from an `if', `then' or `else' keyword
in Caml mode."
  :group 'caml :type 'integer)

(defcustom caml-let-indent caml-default-indent
  "*How many spaces to indent from a `let' keyword."
  :group 'caml :type 'integer)

(defcustom caml-in-indent caml-default-indent
  "*How many spaces to indent from a `in' keyword.
A lot of people like formatting `let' ... `in' expressions whithout
indentation:
        let x = 0 in
        blah x
Set this variable to 0 to get this behaviour.
However, nested declarations are always correctly handled:
        let x = 0 in                             let x = 0
        let y = 0 in              or             in let y = 0
        let z = 0 ...                            in let z = 0 ..."
  :group 'caml :type 'integer)

(defcustom caml-match-indent caml-default-indent
  "*How many spaces to indent from a `match' keyword."
  :group 'caml :type 'integer)

(defcustom caml-try-indent caml-default-indent
  "*How many spaces to indent from a `try' keyword."
  :group 'caml :type 'integer)

(defcustom caml-with-indent caml-default-indent
  "*How many spaces to indent from a `with' keyword."
  :group 'caml :type 'integer)

(defcustom caml-rule-indent caml-default-indent
  "*How many spaces to indent from a `rule' keyword."
  :group 'caml :type 'integer)

(defcustom caml-parse-indent caml-default-indent
  "*How many spaces to indent from a `parse' keyword."
  :group 'caml :type 'integer)

(defcustom caml-parser-indent caml-default-indent
  "*How many spaces to indent from a `parser' keyword."
  :group 'caml :type 'integer)

(defcustom caml-type-indent caml-default-indent
  "*How many spaces to indent from a `type' keyword."
  :group 'caml :type 'integer)

(defcustom caml-val-indent caml-default-indent
  "*How many spaces to indent from a `val' keyword."
  :group 'caml :type 'integer)

;; automatic indentation
;; using abbrev-mode and electric keys

(defcustom caml-use-abbrev-mode t
  "*Non-nil means electrically indent lines starting with leading keyword
such as `end', `done', `else' etc. It makes use of abbrev-mode.

Many people find eletric keywords irritating, so you can disable them in
setting this variable to nil."
  :group 'caml :type 'boolean
  :set '(lambda (var val)
	  (setq caml-use-abbrev-mode val)
	  (abbrev-mode val)))
(make-variable-buffer-local 'caml-use-abbrev-mode)

(defcustom caml-electric-indent t
  "*Non-nil means electrically indent lines starting with `|', `)', `]' or `}'.

Many people find eletric keys irritating, so you can disable them in
setting this variable to nil."
  :group 'caml :type 'boolean)
(make-variable-buffer-local 'caml-electric-indent)

(defcustom caml-electric-close-vector t
  "*Non-nil means electrically insert a `|' before a vector-closing `]',
a `>' before a stream-closing `]' or before an object-closing `}'.

Many people find eletric keys irritating, so you can disable them in
setting this variable to nil. You should probably have this on,
though, if you also have caml-electric-indent on."
  :group 'caml :type 'boolean)
(make-variable-buffer-local 'caml-electric-close-vector)

;; Caml-Interactive
;; configure via `caml-mode-hook'

(defcustom caml-skip-after-eval-phrase t
  "*Non-nil means skip to the end of the phrase after evaluation in the
Caml toplevel."
  :group 'caml :type 'boolean)

(defcustom caml-interactive-read-only-input nil
  "*Non-nil means input send to the Caml toplevel is read-only."
  :group 'caml :type 'boolean)

(defcustom caml-interactive-echo-phrase t
  "*Non-nil means echo phrases in the toplevel buffer when sending
them to the Caml toplevel."
  :group 'caml :type 'boolean)

(defcustom caml-interactive-input-font-lock t
  "*Non nil means Font-Lock for toplevel input phrases."
  :group 'caml :type 'boolean)

(defcustom caml-interactive-output-font-lock t
  "*Non nil means Font-Lock for toplevel output messages."
  :group 'caml :type 'boolean)

(defcustom caml-interactive-error-font-lock t
  "*Non nil means Font-Lock for toplevel error messages."
  :group 'caml :type 'boolean)

(defcustom caml-manual-url "http://pauillac.inria.fr/ocaml/htmlman/index.html"
  "*URL to the Caml reference manual."
  :group 'caml :type 'string)

(defcustom caml-browser 'caml-netscape-manual
  "*Name of function that displays the Caml reference manual.
Valid names are `caml-netscape-manual', `caml-mmm-manual'
and `caml-xemacs-w3-manual' (XEmacs only)."
  :group 'caml)

(defcustom caml-library-path "/usr/local/lib/ocaml/"
  "*Path to the Caml library."
  :group 'caml :type 'string)

(defcustom caml-definitions-max-items 30
  "*Maximum number of items a definitions menu can contain."
  :group 'caml :type 'integer)

(defvar caml-options-list
  '(("Lazy parentheses indentation" . 'caml-lazy-paren)
    ("Lazy `=' indentation" . 'caml-lazy-=)
    ("Force indentation after `let'" . 'caml-let-always-indent)
    "---"
    ("Automatic indentation of leading keywords" . 'caml-use-abbrev-mode)
    ("Electric indentation of ), ] and }" . 'caml-electric-indent)
    ("Electric matching of [|, [< and {<" . 'caml-electric-close-vector)
    "---"
    ("Indent body of comments" . 'caml-indent-comments)
    ("Indent first line of comments" . 'caml-indent-leading-comments)
    ("Leading-`*' comment style" . 'caml-support-leading-star-comments))
  "*List of menu-configurable Caml options")

(defvar caml-interactive-options-list
  '(("Skip phrase after evaluation" . 'caml-skip-after-eval-phrase)
    ("Echo phrase in interactive buffer" . 'caml-interactive-echo-phrase)
    "---"
    ("Font-lock interactive input" . 'caml-interactive-input-font-lock)
    ("Font-lock interactive output" . 'caml-interactive-output-font-lock)
    ("Font-lock interactive error" . 'caml-interactive-error-font-lock)
    "---"
    ("Read only input (XEmacs)" . 'caml-interactive-read-only-input))
  "*List of menu-configurable Caml options")

(defvar caml-interactive-program "ocaml"
  "*Default program name for invoking a Caml toplevel from Emacs.")
(make-variable-buffer-local 'caml-interactive-program)

;; customizable faces for Font-Lock mode

(defgroup caml-faces nil
  "Special faces for the Caml mode.

Face description is the following:
  color for LIGHT backgrounds,
  color for DARK backgrounds,
  ITALIC font, font+color fontification mode,
  BOLD font, font+color fontification mode,
  ITALIC font, font-only fontification mode,
  BOLD font, font-only fontification mode."
  :group 'caml)

(defcustom caml-font-lock-governing '("darkorange3" "orange" nil t nil t)
  "Face description for governing/leading keywords."
  :group 'caml-faces)

(defcustom caml-font-lock-operator '("brown" "khaki" nil nil nil nil)
  "Face description for all toplevel errors."
  :group 'caml-faces)

(defcustom caml-font-lock-interactive-output '("blue4" "cyan" nil nil t nil)
  "Face description for all toplevel outputs."
  :group 'caml-faces)

(defcustom caml-font-lock-interactive-error '("firebrick" "plum1" t t t t)
  "Face description for all toplevel errors."
  :group 'caml-faces)

;; end of customizable variables

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Font-Lock

(defvar caml-fontify-begin nil "Temporary for fontification.")
(make-variable-buffer-local 'caml-fontify-begin)
(defvar caml-fontify-end nil "Temporary for fontification.")
(make-variable-buffer-local 'caml-fontify-end)

(if (and (featurep 'font-lock)
	 (or caml-window-system caml-with-xemacs))
    (progn
      (defun caml-pre-idle-hook ()
	(if (eq major-mode 'caml-mode)
	    (condition-case nil
		(if caml-fontify-begin
		    (caml-fontify caml-fontify-begin caml-fontify-end))
	      (error (warn "Error caught in `caml-pre-idle-hook'")))))

      (defun caml-after-change-fontify (begin end length)
	(caml-pre-idle-hook)) ; old Emacs Font-Lock way of life

      (defun caml-fontify-buffer ()
	(font-lock-default-fontify-buffer)
	(caml-fontify (point-min) (point-max)))

      (defun caml-fontify (start end)
	(save-excursion
	  (let ((modified (buffer-modified-p))) ; hack for Emacs (see below)
	    (goto-char start) (beginning-of-line) (setq start (point))
	    (goto-char end) (end-of-line) (setq end (point))
	    (while (> end start)
	      (goto-char (1- end))
	      (caml-in-literal-or-comment)
	      (cond
	       ((cdr caml-last-loc)
		(caml-beginning-of-literal-or-comment)
		(put-text-property (max start (point)) end 'face
				   (if (looking-at "(\\*[tT][eE][xX]")
				       'font-lock-doc-string-face
				     'font-lock-comment-face))
		(setq end (1- (point))))
	       ((car caml-last-loc)
		(caml-beginning-of-literal-or-comment)
		(put-text-property (max start (point)) end 'face
				   'font-lock-string-face)
		(setq end (point)))
	       (t (while (and caml-cache-local
			      (or (> (caar caml-cache-local) end)
				  (eq 'b (cadar caml-cache-local))))
		    (setq caml-cache-local (cdr caml-cache-local)))
		  (setq end (if caml-cache-local
				(caar caml-cache-local) start)))))
	    (if (not (or caml-with-xemacs modified)) ; Emacs takes properties
		(set-buffer-modified-p nil)))))        ; too seriously...

      (defun caml-font-lock-hook ()
	"Function called by `font-lock-mode' for initialization purposes."
	(if (eq major-mode 'caml-mode)
	    (progn
	      (if (boundp 'pre-idle-hook)
		  (add-hook 'pre-idle-hook 'caml-pre-idle-hook t)
		(add-hook 'after-change-functions
			  'caml-after-change-fontify t))
	      (caml-set-font-lock-faces))))

      (defun caml-make-face-italic (face)
	(condition-case nil (make-face-italic face) (error nil)))
      (defun caml-make-face-bold (face)
	(condition-case nil (make-face-bold face) (error nil)))
      (defun caml-make-face-unitalic (face)
	(condition-case nil (make-face-unitalic face) (error nil)))
      (defun caml-make-face-unbold (face)
	(condition-case nil (make-face-unbold face) (error nil)))

      (defun caml-make-face (name)
	(let ((desc (eval (intern name)))
	      (face (intern (concat name "-face"))))
	  (if (or (and (functionp 'find-face) (find-face face))
		  (facep face)) ()
	    (make-face face)
	    (set-face-foreground
	     face (if use-fonts default-color
		    (if light-bg (nth 0 desc) (nth 1 desc))))
	    (if use-fonts
		(progn
		  (if (nth 4 desc) (caml-make-face-italic face)
		    (caml-make-face-unitalic face))
		  (if (nth 5 desc) (caml-make-face-bold face)
		    (caml-make-face-unbold face)))
	      (if (nth 2 desc) (caml-make-face-italic face)
		(caml-make-face-unitalic face))
	      (if (nth 3 desc) (caml-make-face-bold face)
		(caml-make-face-unbold face))))))

      (defun caml-set-font-lock-faces ()
	"Set faces for Font-Lock mode."
	(let* ((use-fonts
		(or (and (boundp 'font-lock-use-fonts)
			 font-lock-use-fonts
			 (not (and (boundp 'font-lock-use-colors)
				   font-lock-use-colors)))
		    (and (fboundp 'device-class)
			 (eq (device-class) 'mono))
		    (and (not (fboundp 'device-class))
			 (fboundp 'x-display-color-p)
			 (not (x-display-color-p)))))
	       (light-bg
		(if caml-window-system
		    (if caml-with-xemacs
			(if (and (fboundp 'color-rgb-components)
				 (< (apply '+ (color-rgb-components
					       (make-color-specifier
						[default background])))
				    (* (apply '+
					      (color-rgb-components
					       (make-color-specifier "white")))
				       0.6))) nil t)
		      (let ((param (cdr (assq 'background-color
					      (frame-parameters)))))
			(cond
			 ((boundp 'font-lock-background-mode)
			  (if (eq font-lock-background-mode 'dark) nil t))
			 ((eq system-type 'ms-dos)
			  (if (string-match "light" param) t nil))
			 ((and param (fboundp 'x-color-values)
			       (< (apply '+ (x-color-values param))
				  (* (apply '+ (x-color-values "white"))
				     0.6))) nil)
			 (t t))))))
	       (default-color (if light-bg "black" "white")))
	  (mapcar 'caml-make-face
		  '("caml-font-lock-governing"
		    "caml-font-lock-operator"
		    "caml-font-lock-interactive-output"
		    "caml-font-lock-interactive-error"))))

      (defvar caml-font-lock-keywords
	'(("^#[ \t]*[a-z][_a-z]*\\>\\|\\<\\(external\\|open\\)\\>"
	   0 'font-lock-preprocessor-face nil)
	  ("\\<\\(s\\(ig\\|truct\\)\\|module\\([ \t\n]+type\\)?\\|functor\\|\\(with\\|and\\|let\\)[ \t\n]+\\(type\\|module\\)\\)\\>"
	   0 'caml-font-lock-governing-face nil)
	  ("\\<\\(val\\|type\\|method\\|constraint\\|class\\|in\\|inherit\\|initializer\\|let\\|rec\\|and\\|begin\\|object\\|end\\)\\>"
	   0 'caml-font-lock-governing-face nil)
	  ("\\<\\(as\\|do\\(ne\\|wnto\\)?\\|else\\|for\\|if\\|let\\|m\\(atch\\|utable\\)\\|new\\|p\\(arser\\|rivate\\)\\|t\\(hen\\|o\\|ry\\)\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\|lazy\\|virtual\\|exception\\|raise\\|failwith\\|assert\\|fun\\(ction\\)?\\)\\>"
	   0 'font-lock-keyword-face nil)
	  ("\\<open\\>[ \t\n]*\\([_A-Za-z\277-\377]\\(\\w\\|\\.\\)*\\)"
	   1 'font-lock-function-name-face nil)
	  ("\\<\\(\\(method\\([ \t\n]+private\\)?\\)\\([ \t\n]+virtual\\)?\\|val\\([ \t\n]+mutable\\)?\\|constraint\\|external\\|and\\|let\\([ \t\n]+rec\\)?\\)\\>[ \t]*\\(\\(\\<['_A-Za-z\277-\377~?]\\w*\\>:?[ \t]*\\)*\\)\\>"
	   7 'font-lock-variable-name-face nil)
	  ("\\<\\(\\(class\\([ \t\n]+type\\)?\\)\\([ \t\n]+virtual\\)?\\|inherit\\|module\\([ \t\n]+type\\)?\\|type\\)\\>[ \t]*\\(\\(\\<['_A-Za-z\277-\377~?]\\w*\\>:?[ \t]*\\)*\\)\\>"
	   6 'font-lock-type-face nil)
	  ("\\([?~]?[_A-Za-z\277-\377]\\w*[ \t\n]*:\\)[^:>=]"
	   1 'font-lock-variable-name-face nil)
	  ("\\<exception\\>[ \t]*\\(\\<[_A-Za-z\277-\377]\\w*\\>\\)"
	   1 'font-lock-variable-name-face nil)
	  ("\\<\\(as[lr]\\|false\\|l\\(and\\|xor\\|or\\|s[lr]\\)mod\\|not\\|ref\\|o[fr]\\|true\\|unit\\)\\>"
	   0 'font-lock-reference-face nil)
	  ("[][;,()|{}@^!:#*=<>&/%+~?---]\\.?\\|\\.\\."
	   0 'caml-font-lock-operator-face nil))
	"Font-Lock patterns for Caml mode.")

      (if (featurep 'sym-lock)
	  ;; to change this table, xfd -fn '-adobe-symbol-*--12-*' may be
	  ;; used to determine the symbol character codes.
	  (defvar caml-sym-lock-keywords
	    '(("<-" 0 1 172)
	      ("->" 0 1 174)
	      (":=" 0 1 220)
	      ("<=" 0 1 163)
	      (">=" 0 1 179)
	      ("<>" 0 1 185)
	      ("==" 0 1 186)
	      ("||" 0 1 218)
	      ("&&" 0 1 217)
	      ("[^*]\\(\\*\\)\\." 1 8 180)
	      ("\\(/\\)\\." 1 3 184)
	      (":>" 0 1 202)
	      (";;" 0 1 191)
	      ;; ("\\<_\\>" 0 3 188)
	      ("\\<sqrt\\>" 0 3 214)
	      ("\\<unit\\>" 0 3 198)
	      ;; ("\\<fun\\>" 0 3 108)
	      ("\\<or\\>" 0 3 218)
	      ("\\<not\\>" 0 3 216))
	    "If non nil: Overrides default Sym-Lock patterns for Caml."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Keymap

(defvar caml-mode-map nil
  "Keymap used in Caml mode.")
(setq caml-mode-map (make-sparse-keymap))
(define-key caml-mode-map "#" 'caml-electric)
(define-key caml-mode-map "%" 'caml-electric)
(define-key caml-mode-map "|" 'caml-electric)
(define-key caml-mode-map ")" 'caml-electric)
(define-key caml-mode-map "}" 'caml-electric-rc)
(define-key caml-mode-map "]" 'caml-electric-rb)
(define-key caml-mode-map "\t" 'caml-indent-command)
(define-key caml-mode-map "\M-\C-h" 'caml-mark-phrase)
(define-key caml-mode-map "\M-q" 'caml-indent-phrase)
(define-key caml-mode-map "\C-c\C-q" 'caml-indent-phrase)
(define-key caml-mode-map "\C-c\C-a" 'caml-find-alternate-file)
(define-key caml-mode-map "\C-c\C-c" 'compile)
(define-key caml-mode-map "\M-\C-x" 'caml-eval-phrase)
(define-key caml-mode-map "\C-x\C-e" 'caml-eval-phrase)
(define-key caml-mode-map "\C-c\C-e" 'caml-eval-phrase)
(define-key caml-mode-map "\C-c\C-r" 'caml-eval-region)
(define-key caml-mode-map "\C-c\C-b" 'caml-eval-buffer)
(define-key caml-mode-map "\C-c\C-s" 'caml-run-caml)
(define-key caml-mode-map "\C-c\C-i" 'caml-interrupt-caml)
(define-key caml-mode-map "\C-c\C-k" 'caml-kill-caml)
(define-key caml-mode-map "\C-c\C-n" 'caml-next-phrase)
(define-key caml-mode-map "\C-c\C-p" 'caml-previous-phrase)
(define-key caml-mode-map [(meta control down)]  'caml-next-phrase)
(define-key caml-mode-map [(meta control up)] 'caml-previous-phrase)
(define-key caml-mode-map "\C-c`" 'caml-interactive-next-error-source)
(define-key caml-mode-map "\C-cb" 'caml-insert-begin-form)
(define-key caml-mode-map "\C-cf" 'caml-insert-for-form)
(define-key caml-mode-map "\C-cw" 'caml-insert-while-form)
(define-key caml-mode-map "\C-ci" 'caml-insert-if-form)
(define-key caml-mode-map "\C-cl" 'caml-insert-let-form)
(define-key caml-mode-map "\C-cm" 'caml-insert-match-form)
(define-key caml-mode-map "\C-ct" 'caml-insert-try-form)

(defvar caml-mode-syntax-table ()
  "Syntax table in use in Caml mode buffers.")
(setq caml-mode-syntax-table (make-syntax-table))
(modify-syntax-entry ?_ "w" caml-mode-syntax-table)
(modify-syntax-entry ?? "w" caml-mode-syntax-table)
(modify-syntax-entry ?~ "w" caml-mode-syntax-table)
(modify-syntax-entry ?: "." caml-mode-syntax-table)
(modify-syntax-entry ?' "w" caml-mode-syntax-table)
;; ' is part of words (for primes)
(modify-syntax-entry ?` "\"" caml-mode-syntax-table)
;; ` is a string delimiter (camllight compatibility)
(modify-syntax-entry ?\" "\"" caml-mode-syntax-table)
;; " is a string delimiter
(modify-syntax-entry ?\\ "\\" caml-mode-syntax-table)
(modify-syntax-entry ?\( "()1" caml-mode-syntax-table)
(modify-syntax-entry ?*  ".23" caml-mode-syntax-table)
(modify-syntax-entry ?\) ")(4" caml-mode-syntax-table)
(let ((i 192))
  (while (< i 256)
    (modify-syntax-entry i "w" caml-mode-syntax-table)
    (setq i (1+ i))))

(defconst caml-font-lock-syntax
  '((?` . ".") (?\" . "."))
  "Syntax changes for Font-Lock.")

(defvar caml-mode-abbrev-table ()
  "Abbrev table used for Caml mode buffers.")
(defun caml-define-abbrev (keyword)
  (define-abbrev caml-mode-abbrev-table keyword keyword 'caml-abbrev-hook))
(if caml-mode-abbrev-table ()
  (setq caml-mode-abbrev-table (make-abbrev-table))
  (mapcar 'caml-define-abbrev
	  '("module" "class" "object" "type" "val" "inherit" "virtual"
	    "constraint" "exception" "external" "open" "method" "and"
	    "initializer" "to" "downto" "do" "done" "else" "begin" "end"
	    "let" "in" "then" "with"))
  (setq abbrevs-changed nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              The major mode

(defun caml-mode ()
  "Major mode for editing Caml code.

Dedicaced to Emacs and XEmacs, version 20 and higher. Provides
automatic indentation and compilation interface. Performs font/color
highlighting using Font-Lock. It is designed for Objective Caml but
handles Objective Labl and Camllight as well.

Report bugs, remarks and questions to Albert.Cohen@prism.uvsq.fr.

If you use XEmacs, the Font-Lock minor-mode is used accordingly to
your customization options. Within Emacs, you may want to use
Font-Lock in addding the following lines to the configuration file:

  (if (and (boundp 'window-system) window-system)
      (require 'font-lock))

Within XEmacs (more generally, if you have variable-sized fonts
and `atomic-extents' supported) you may also want to use Sym-Lock:

  (if (and (boundp 'window-system) window-system
	   (string-match \"XEmacs\" emacs-version))
      (require 'sym-lock))

Emacs supports neither variable-sized fonts nor atomic sequences, perhaps
it is a good occasion for you to try XEmacs!

You have better byte-compile caml.el (and sym-lock.el if you use it)
because symbol highlighting is very time consuming.

For customization purposes, you should use `caml-mode-hook'
(run for every file) or `caml-load-hook' (run once) and not patch
the mode itself. You should add to your configuration file something like:
  (add-hook 'caml-mode-hook
            '(lambda ()
               ... ; your customization code
             ))
For example you can change the indentation of some keywords, the
`electric' flags, Font-Lock colors... Every customizable variable is
documented, use `C-h-v' or look at the mode's source code.

A special case is Sym-Lock customization: You may set
`caml-sym-lock-keywords' in your `.emacs' configuration file
to override default Sym-Lock patterns.

`custom-caml.el' is a sample customization file for standard changes.
You can append it to your `.emacs' or use it as a tutorial.

`M-x camldebug' FILE starts the Caml debugger camldebug on the executable
FILE, with input and output in an Emacs buffer named *camldebug-FILE*.

A Caml Interactive Mode to evaluate expressions in a toplevel is included.
Type `M-x caml-run-caml' or see special-keys below.

Some elementary rules have to be followed in order to get the best of
indentation facilities.
  - Because the `function' keyword has a special indentation (to handle
    case matchs) use the `fun' keyword when no case match is done.
  - Prefer the `or' keyword to `||' (they are semantically equivalent),
    it avoids some unwanted electric indentations.
  - In OCaml, `;;' is no longer necessary for correct indentation,
    except before top level phrases not introduced by `type', `val', `let'
    etc. (i.e. phrases used for their side-effects or to be executed
    in a top level.)
  - Long sequences of `and's may slow down indentation slightly, since
    some computations (few actually) require to go back to the
    beginning of the sequence. Some very long nested blocks may also lead
    to slow processing of `end's, `else's, `done's...
  - Multiline strings are handled properly, but the string concatenation `^'
    is preferred to break long strings (the C-j keystroke can help in some
    cases).

Known bugs:
  - With Emacs: Text highlighting may be incorrect in some tricky examples
    (nested comments and strings), but there exists no proper solution
    to this without rewritting Font-Lock algorithms. This has been done
    with the improved XEmacs Font-Lock interface: Use XEmacs!
  - When writting a line with mixed code and comments, avoid putting
    comments at the beginning or middle of the text. More precisely, 
    writing comments immediately after `=' or parentheses then writing
    some more code on the line leads to indentation errors: You may write
    `let x (* blah *) = blah' but should avoid `let x = (* blah *) blah'.
    I'll try to fix this, even if I did not get any bug report about
    it so far...

Special keys for Caml mode:\\{caml-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq case-fold-search nil)
  (setq major-mode 'caml-mode)
  (setq mode-name "Caml")
  (use-local-map caml-mode-map)
  (set-syntax-table caml-mode-syntax-table)
  (setq local-abbrev-table caml-mode-abbrev-table)

  (if caml-window-system (caml-build-menu))

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|\\*)$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+[ \t]*")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'caml-indent-command)
  (make-variable-buffer-local 'before-change-functions)
  (add-hook 'before-change-functions 'caml-before-change-function)
  (make-variable-buffer-local 'after-change-functions)
  (add-hook 'after-change-functions 'caml-after-change-function)
  (make-variable-buffer-local 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'caml-auto-fill-function)
	 
  ;; hooks for caml-mode, use them for caml-mode configuration
  (run-hooks 'caml-mode-hook)
  (caml-install-font-lock)
  (if caml-use-abbrev-mode (abbrev-mode 1))
  (message (concat "Major mode for Caml programs, "
		   caml-mode-version ".")))

(defun caml-install-font-lock ()
  (if (and (featurep 'font-lock)
	   (or caml-window-system caml-with-xemacs))
      (progn
	(caml-set-font-lock-faces)
	(if (featurep 'sym-lock) ; needed AFTER caml-set-font-lock-faces
	    (progn
	      (setq sym-lock-color
		    (face-foreground 'caml-font-lock-operator-face))
	      (if (not sym-lock-keywords)
		  (sym-lock caml-sym-lock-keywords))))
	(add-hook 'font-lock-mode-hook 'caml-font-lock-hook)
	(make-variable-buffer-local 'font-lock-defaults)
	(setq font-lock-defaults
	      (list 'caml-font-lock-keywords t nil
		    caml-font-lock-syntax nil))
	(make-variable-buffer-local 'font-lock-fontify-buffer-function)
	(setq font-lock-fontify-buffer-function 'caml-fontify-buffer)
	(font-lock-set-defaults)
	(if (not (or caml-with-xemacs font-lock-mode))
	    (font-lock-mode 1)) ; useful for beginners if not standard
	'font-lock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Error processing

(require 'compile)

;; In some versions of Emacs, the regexps in
;; compilation-error-regexp-alist do not match the error messages when
;; the language is not English. Hence we add a regexp.

(defconst caml-error-regexp
  "^[A-\377]+ \"\\([^\"\n]+\\)\", [A-\377]+ \\([0-9]+\\)[-,:]"
  "Regular expression matching the error messages produced by (o)camlc.")

(if (boundp 'compilation-error-regexp-alist)
    (or (assoc caml-error-regexp
               compilation-error-regexp-alist)
        (setq compilation-error-regexp-alist
              (cons (list caml-error-regexp 1 2)
               compilation-error-regexp-alist))))

;; A regexp to extract the range info.

(defconst caml-error-chars-regexp
  ".*, .*, [A-\377]+ \\([0-9]+\\)-\\([0-9]+\\):"
  "Regular expression extracting the character numbers
from an error message produced by (o)camlc.")

;; Wrapper around next-error.

;; itz 04-21-96 somebody didn't get the documentation for next-error
;; right. When the optional argument is a number n, it should move
;; forward n errors, not reparse.

;; itz 04-21-96 instead of defining a new function, use defadvice
;; that way we get our effect even when we do \C-x` in compilation buffer  

(defadvice next-error (after caml-next-error activate)
 "Reads the extra positional information provided by the Caml compiler.

Puts the point and the mark exactly around the erroneous program
fragment. The erroneous fragment is also temporarily highlighted if
possible."
 (if (eq major-mode 'caml-mode)
     (let ((beg nil) (end nil))
       (save-excursion
	 (set-buffer compilation-last-buffer)
	 (save-excursion
	   (goto-char (window-point (get-buffer-window (current-buffer))))
	   (if (looking-at caml-error-chars-regexp)
	       (setq beg (string-to-int (caml-match-string 1))
		     end (string-to-int (caml-match-string 2))))))
       (beginning-of-line)
       (if beg
	   (progn
	     (setq beg (+ (point) beg) end (+ (point) end))
	     (goto-char beg) (push-mark end t t))))))

(defvar caml-interactive-error-regexp
  (concat "\\(\\("
	  "Toplevel input:"
	  "\\|Entr.e interactive:"
	  "\\|Characters [0-9-]*:"
	  "\\|The global value [^ ]* is referenced before being defined."
	  "\\|La valeur globale [^ ]* est utilis.e avant d'.tre d.finie."
	  "\\|Reference to undefined global"
	  "\\|The C primitive \"[^\"]*\" is not available."
	  "\\|La primitive C \"[^\"]*\" est inconnue."
	  "\\|Cannot find \\(the compiled interface \\)?file"
	  "\\|L'interface compil.e [^ ]* est introuvable."
	  "\\|Le fichier [^ ]* est introuvable."
	  "\\|Exception non rattrap.e:"
	  "\\|Uncaught exception:"
	  "\\)[^#]*\\)" )
  "Regular expression matching the error messages produced by Caml.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Support definitions

(defun caml-auto-fill-function ()
  (if (not (caml-in-literal-p))
      (do-auto-fill)))

(defun caml-forward-char (&optional step)
  (if step (goto-char (+ (point) step))
    (goto-char (1+ (point)))))

(defun caml-backward-char (&optional step)
  (if step (goto-char (- (point) step))
    (goto-char (1- (point)))))

(defun caml-in-indentation-p ()
  "Tests whether all characters between beginning of line and point
are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defvar caml-cache-stop (point-min))
(make-variable-buffer-local 'caml-cache-stop)
(defvar caml-cache nil)
(make-variable-buffer-local 'caml-cache)
(defvar caml-cache-local nil)
(make-variable-buffer-local 'caml-cache-local)
(defvar caml-cache-last-local nil)
(make-variable-buffer-local 'caml-cache-last-local)
(defvar caml-last-loc (cons nil nil))

(defun caml-after-change-function (begin end length)
  (setq caml-fontify-begin begin) ; automatic fontification
  (setq caml-fontify-end end))    ; of strings and comments

(defun caml-before-change-function (begin end)
  (setq caml-cache-stop (min caml-cache-stop (1- begin))))

(defun caml-in-literal-p ()
  "Returns non-nil if point is inside a Caml literal."
  (car (caml-in-literal-or-comment)))
(defun caml-in-comment-p ()
  "Returns non-nil if point is inside a Caml comment."
  (cdr (caml-in-literal-or-comment)))
(defun caml-in-literal-or-comment-p ()
  "Returns non-nil if point is inside a Caml literal or comment."
  (caml-in-literal-or-comment)
  (or (car caml-last-loc) (cdr caml-last-loc)))
(defun caml-in-literal-or-comment ()
  "Returns the pair `((caml-in-literal-p) . (caml-in-comment-p))'."
  (if (and (<= (point) caml-cache-stop) caml-cache)
      (progn
	(if (or (not caml-cache-local) (not caml-cache-last-local)
		(and (>= (point) (caar caml-cache-last-local))))
	    (setq caml-cache-local caml-cache))
	(while (and caml-cache-local (< (point) (caar caml-cache-local)))
	  (setq caml-cache-last-local caml-cache-local
		caml-cache-local (cdr caml-cache-local)))
	(setq caml-last-loc
	      (if caml-cache-local
		  (cons (eq (cadar caml-cache-local) 'b)
			(> (cddar caml-cache-local) 0))
		(cons nil nil))))
    (let ((flag t) (op (point)) (mp (min (point) (1- (point-max))))
	  (literal nil) (balance 0) (end-of-comment nil))
      (while (and caml-cache (<= caml-cache-stop (caar caml-cache)))
	(setq caml-cache (cdr caml-cache)))
      (if caml-cache
	  (if (eq (cadar caml-cache) 'b)
	      (progn
		(setq caml-cache-stop (1- (caar caml-cache)))
		(goto-char caml-cache-stop)
		(setq balance (cddar caml-cache))
		(setq caml-cache (cdr caml-cache)))
	    (setq balance (cddar caml-cache))
	    (setq caml-cache-stop (caar caml-cache))
	    (goto-char caml-cache-stop)
	    (skip-chars-forward "("))
	(goto-char caml-cache-stop))
      (skip-chars-backward "\\\\*")
      (while flag
	(setq literal nil)
	(if end-of-comment (setq balance 0 end-of-comment nil))
	(skip-chars-forward "^\\\\'`\"(\\*")
	(cond
	 ((looking-at "\\\\")
	  (caml-forward-char 2))
	 ((looking-at "'\\([^\n']\\|\\\\..?.?\\)'")
	  (caml-forward-char)
	  (setq caml-cache (cons (cons (point) (cons 'b balance))
				   caml-cache))
	  (skip-chars-forward "^'") (caml-forward-char)
	  (setq caml-cache (cons (cons (point) (cons 'e balance))
				   caml-cache))
	  (setq literal t))
	 ((looking-at "`\\([^\n']\\|\\\\..?.?\\)`")
	  (caml-forward-char)
	  (setq caml-cache (cons (cons (point) (cons 'b balance))
				   caml-cache))
	  (skip-chars-forward "^`") (caml-forward-char)
	  (setq caml-cache (cons (cons (point) (cons 'e balance))
				   caml-cache))
	  (setq literal t))
	 ((looking-at "\"")
	  (caml-forward-char)
	  (setq caml-cache (cons (cons (point) (cons 'b balance))
				   caml-cache))
	  (skip-chars-forward "^\\\\\"")
	  (while (looking-at "\\\\")
	    (caml-forward-char 2) (skip-chars-forward "^\\\\\""))
	  (caml-forward-char)
	  (setq caml-cache (cons (cons (point) (cons 'e balance))
				   caml-cache))
	  (setq literal t))
	 ((looking-at "(\\*")
	  (setq balance (1+ balance))
	  (setq caml-cache (cons (cons (point) (cons nil balance))
				   caml-cache))
	  (caml-forward-char 2))
	 ((looking-at "\\*)")
	  (caml-forward-char 2)
	  (if (> balance 1)
	      (prog2
		  (setq balance (1- balance))
		  (setq caml-cache (cons (cons (point) (cons nil balance))
					   caml-cache)))
	    (setq end-of-comment t)
	    (setq caml-cache (cons (cons (point) (cons nil 0))
				     caml-cache))))
	 (t (caml-forward-char)))
	(setq flag (<= (point) mp)))
      (setq caml-cache-local caml-cache
	    caml-cache-stop (point))
      (goto-char op)
      (if caml-cache (caml-in-literal-or-comment) 
	(setq caml-last-loc (cons nil nil))
	caml-last-loc))))

(defun caml-beginning-of-literal-or-comment ()
  "Skips to the beginning of the current literal or comment (or buffer)."
  (interactive)
  (if (caml-in-literal-or-comment-p)
      (caml-beginning-of-literal-or-comment-fast)))

(defun caml-beginning-of-literal-or-comment-fast ()
  (while (and caml-cache-local
	      (or (eq 'b (cadar caml-cache-local))
		  (> (cddar caml-cache-local) 0)))
    (setq caml-cache-last-local caml-cache-local
	  caml-cache-local (cdr caml-cache-local)))
  (if caml-cache-last-local
      (goto-char (caar caml-cache-last-local))
    (goto-char (point-min)))
  (if (eq 'b (cadar caml-cache-last-local)) (caml-backward-char)))

(defun caml-false-=-p ()
  "Is the underlying `=' the second letter of an operator?"
  (or (char-equal ?: (preceding-char))
      (char-equal ?> (preceding-char))
      (char-equal ?< (preceding-char))
      (char-equal ?= (preceding-char))))

(defun caml-at-phrase-break-p ()
  "Is the underlying `;' a phrase break?"
  (and (char-equal ?\; (following-char))
       (or (and (not (eobp))
		(char-equal ?\; (char-after (1+ (point)))))
	   (char-equal ?\; (preceding-char)))))

(defun caml-backward-up-list ()
  "Safe up-list regarding comments, literals and errors."
  (let ((balance 1) (op (point)) (oc nil))
    (caml-in-literal-or-comment)
    (while (and (> (point) (point-min)) (> balance 0))
      (setq oc (if caml-cache-local (caar caml-cache-local) (point-min)))
      (condition-case nil (up-list -1) (error (goto-char (point-min))))
      (if (>= (point) oc) (setq balance (1- balance))
	(goto-char op)
	(skip-chars-backward "^[]{}()") (caml-backward-char)
	(if (not (caml-in-literal-or-comment-p))
	    (cond
	     ((looking-at "[[{(]")
	      (setq balance (1- balance)))
	     ((looking-at "[]})]")
	      (setq balance (1+ balance))))
	  (caml-beginning-of-literal-or-comment-fast)))
      (setq op (point)))))

(defun caml-assoc-indent (kwop &optional looking-let)
  "Returns relative indentation of the keyword given in argument."
  (let ((ind (symbol-value (cdr (assoc kwop caml-keyword-alist)))))
    (if (string-match "\\<\\(with\\|function\\|parser?\\)\\>" kwop)
	(+ (if (and caml-let-always-indent
		    looking-let (< ind caml-let-indent))
	       caml-let-indent ind) caml-|-extra-unindent) ind)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Indentation stuff

(defconst caml-keyword-regexp "\\<\\(object\\|initializer\\|and\\|c\\(onstraint\\|lass\\)\\|m\\(atch\\|odule\\|ethod\\|utable\\)\\|s\\(ig\\|truct\\)\\|begin\\|e\\(lse\\|x\\(ception\\|ternal\\)\\)\\|t\\(o\\|hen\\|ry\\|ype\\)\\|v\\(irtual\\|al\\)\\|w\\(h\\(ile\\|en\\)\\|ith\\)\\|i\\(f\\|n\\(herit\\)?\\)\\|f\\(or\\|un\\(ct\\(or\\|ion\\)\\)?\\)\\|let\\|do\\(wnto\\)?\\|parser?\\|rule\\|of\\)\\>\\|->\\|[;,|]"
  "Regexp for all recognized keywords.")

(defconst caml-match-|-keyword-regexp
  "\\<\\(and\\|function\\|type\\|with\\|parser?\\)\\>\\|[[({|=]"
  "Regexp for keywords supporting case match.")

(defconst caml-operator-regexp "[---+*/=<>@^&|~?]\\|:>\\|::\\|\\<\\(or\\|l\\(and\\|x?or\\|s[lr]\\)\\|as[lr]\\|mod\\)\\>"
  "Regexp for all operators.")

(defconst caml-kwop-regexp (concat caml-keyword-regexp "\\|=")
  "Regexp for all keywords, and the = operator which is generally
considered as a special keyword.")

(defconst caml-matching-keyword-regexp
  "\\<\\(and\\|do\\(ne\\)?\\|e\\(lse\\|nd\\)\\|in\\|then\\|\\(down\\)?to\\)\\>"
  "Regexp matching Caml keywords which act as end block delimiters.")

(defconst caml-leading-kwop-regexp
  (concat caml-matching-keyword-regexp "\\|\\<with\\>\\|[|>]?\\]\\|>?}\\|[|)#%]\\|;;")
  "Regexp matching Caml keywords which need special indentation.")

(defconst caml-governing-phrase-regexp
  "\\<\\(val\\|type\\|m\\(ethod\\|odule\\)\\|c\\(onstraint\\|lass\\)\\|in\\(herit\\|itializer\\)\\|ex\\(ternal\\|ception\\)\\|open\\|let\\|object\\)\\>"
  "Regexp matching caml phrase delimitors.")

(defconst caml-governing-phrase-regexp-with-break
  (concat caml-governing-phrase-regexp "\\|;;"))

(defconst caml-keyword-alist
  '(("module" . caml-default-indent)
    ("class" . caml-class-indent)
    ("sig" . caml-sig-struct-indent)
    ("struct" . caml-sig-struct-indent)
    ("method" . caml-method-indent)
    ("object" . caml-begin-indent)
    ("begin" . caml-begin-indent)
    ("for" . caml-for-while-indent)
    ("while" . caml-for-while-indent)
    ("do" . caml-do-indent)
    ("type" . caml-type-indent) ; in some cases, `type' acts like a match
    ("val" . caml-val-indent)
    ("fun" . caml-fun-indent)
    ("if" . caml-if-then-else-indent)
    ("then" . caml-if-then-else-indent)
    ("else" . caml-if-then-else-indent)
    ("let" . caml-let-indent)
    ("match" . caml-match-indent)
    ("try" . caml-try-indent)
    ("rule" . caml-rule-indent)

    ;; case match keywords
    ("function" . caml-function-indent)
    ("with" . caml-with-indent)
    ("parse" . caml-parse-indent)
    ("parser" . caml-parser-indent)

    ;; default indentation keywords
    ("when" . caml-default-indent)
    ("functor" . caml-default-indent)
    ("exception" . caml-default-indent)
    ("inherit" . caml-default-indent)
    ("initializer" . caml-default-indent)
    ("constraint" . caml-default-indent)
    ("virtual" . caml-default-indent)
    ("mutable" . caml-default-indent)
    ("external" . caml-default-indent)
    ("in" . caml-in-indent)
    ("of" . caml-default-indent)
    ("to" . caml-default-indent)
    ("downto" . caml-default-indent)
    ("->" . caml-default-indent)
    ("[" . caml-default-indent)
    ("(" . caml-default-indent)
    ("{" . caml-default-indent)
    ("|" . caml-default-indent))
"Association list of indentation values based on governing keywords.")

(defconst caml-leading-kwop-alist
  '(("|" . caml-find-|-match)
    ("}" . caml-find-match)
    (">}" . caml-find-match)
    (")" . caml-find-match)
    ("]" . caml-find-match)
    ("|]" . caml-find-match)
    (">]" . caml-find-match)
    ("end" . caml-find-match)
    ("done" . caml-find-done-match)
    ("in"  . caml-find-in-match)
    ("with" . caml-find-with-match)
    ("else" . caml-find-else-match)
    ("then" . caml-find-match)
    ("do" . caml-find-do-match)
    ("to" . caml-find-match)
    ("downto" . caml-find-match)
    ("and" . caml-find-and-match))
  "Association list used in Caml mode for skipping back over nested blocks.")

(defun caml-find-meaningful-word ()
  "Look back for a word, skipping comments and blanks.
Returns the actual text of the word, if found."
  (let ((found nil) (kwop nil))
    (while (and (not found)
		(re-search-backward
		 "[^ \t\n'_A-Za-z\277-\377]\\|\\<\\w+\\>\\|\\*)"
		 (point-min) t))
      (setq kwop (caml-match-string 0))
      (if kwop
	  (if (caml-in-comment-p)
	      (caml-beginning-of-literal-or-comment-fast)
	    (setq found t))
	(setq found t)))
    (if found kwop (goto-char (point-min)) nil)))

(defconst caml-find-kwop-regexp
  (concat caml-matching-keyword-regexp "\\|\\<\\(for\\|while\\|do\\|if\\|begin\\|s\\(ig\\|truct\\)\\|object\\)\\>\\|[][(){}]\\|\\*)"))
(defun caml-make-find-kwop-regexp (kwop-regexp)
  (concat caml-find-kwop-regexp "\\|" kwop-regexp))

(defun caml-find-kwop (kwop-regexp &optional do-not-skip-regexp)
  "Look back for a Caml keyword or operator matching KWOP-REGEXP.
Skips blocks etc...

Ignore occurences inside literals and comments.
If found, return the actual text of the keyword or operator."
  (let ((found nil) (kwop nil))
    (while (and (not found)
		(re-search-backward kwop-regexp (point-min) t)
		(setq kwop (caml-match-string 0)))
      (cond
       ((caml-in-literal-or-comment-p)
	(caml-beginning-of-literal-or-comment-fast))
       ((looking-at "[]})]")
	(caml-backward-up-list))
       ((caml-at-phrase-break-p)
	(setq found t))
       ((and do-not-skip-regexp (looking-at do-not-skip-regexp))
	(if (and (string= kwop "|") (char-equal ?| (preceding-char)))
	    (backward-char)
	  (setq found t)))
       ((looking-at caml-matching-keyword-regexp)
	(funcall (cdr (assoc (caml-match-string 0)
			     caml-leading-kwop-alist))))
       (t (setq found t))))
    (if found kwop (goto-char (point-min)) nil)))

(defun caml-find-match ()
  (caml-find-kwop caml-find-kwop-regexp))

(defconst caml-find-with-match-regexp
  (caml-make-find-kwop-regexp
   "\\<\\(match\\|try\\|module\\|begin\\)\\>\\|[[{(]"))
(defun caml-find-with-match ()
  (caml-find-kwop caml-find-with-match-regexp))

(defconst caml-find-in-match-regexp
  (caml-make-find-kwop-regexp "\\<let\\>"))
(defun caml-find-in-match ()
  (let ((kwop (caml-find-kwop caml-find-in-match-regexp "\\<and\\>")))
    (cond ((string= kwop "and") (caml-find-in-match))
	  (t kwop))))

(defconst caml-find-else-match-regexp
  (caml-make-find-kwop-regexp ";"))
(defun caml-find-else-match ()
  (let ((kwop (caml-find-kwop caml-find-else-match-regexp
				     "\\<then\\>")))
    (cond ((string= kwop "then")
	   (caml-find-match) kwop)
	  ((string= kwop ";")
	   (caml-find-semi-colon-match)
	   (caml-find-else-match) kwop))))

(defun caml-find-do-match ()
  (let ((kwop (caml-find-kwop caml-find-kwop-regexp
				   "\\<\\(down\\)?to\\>")))
    (if (or (string= kwop "to") (string= kwop "downto"))
	(caml-find-match) kwop)))

(defun caml-find-done-match ()
  (let ((kwop (caml-find-kwop caml-find-kwop-regexp "\\<do\\>")))
    (if (string= kwop "do")
	(caml-find-do-match) kwop)))

(defconst caml-find-and-match-regexp
  "\\<\\(do\\(ne\\)?\\|e\\(lse\\|nd\\)\\|in\\|then\\|with\\|\\(down\\)?to\\)\\>\\|\\<\\(for\\|while\\|do\\|if\\|try\\|match\\|begin\\|s\\(ig\\|truct\\)\\|class\\)\\>\\|[][(){}]\\|\\*)\\|\\<\\(rule\\|exception\\|let\\|in\\|type\\|val\\|module\\)\\>")
(defconst caml-find-and-match-regexp-dnr
  (concat caml-find-and-match-regexp "\\|\\<and\\>"))
(defun caml-find-and-match (&optional do-not-recurse)
  (let* ((kwop (caml-find-kwop (if do-not-recurse
					  caml-find-and-match-regexp-dnr
					  caml-find-and-match-regexp)
				      "\\<and\\>"))
	 (old-point (point)))
    (cond ((or (string= kwop "type") (string= kwop "module"))
	   (let ((kwop2 (caml-find-meaningful-word)))
	     (cond ((string= kwop2 "with") kwop2)
		   ((string= kwop2 "and") (caml-find-and-match))
		   ((string= kwop "module") (string= kwop2 "let") kwop2)
		   (t (goto-char old-point) kwop))))
	  (t kwop))))

(defconst caml-find-=-match-regexp
  (caml-make-find-kwop-regexp "\\<\\(let\\|m\\(ethod\\|odule\\)\\|type\\|class\\|when\\|i[fn]\\)\\>"))
(defun caml-find-=-match ()
  (let ((kwop (caml-find-kwop caml-find-=-match-regexp
				     "\\<and\\|in\\>")))
    (if (string= kwop "and") (caml-find-and-match) kwop)))

(defun caml-if-when-= ()
  (save-excursion
    (caml-find-=-match)
    (looking-at "\\<\\(if\\|when\\)\\>")))

(defconst caml-find-|-match-regexp
  (caml-make-find-kwop-regexp
   "\\<\\(with\\|function\\|type\\|parser?\\)\\>\\|[=|]"))
(defun caml-find-|-match ()
  (let* ((kwop (caml-find-kwop caml-find-|-match-regexp
				    "\\<\\(and\\|with\\)\\>\\||"))
	 (old-point (point)))
    (cond ((string= kwop "and")
	   (setq old-point (point))
	   (setq kwop (caml-find-and-match))
	   (goto-char old-point)
	   kwop)
	  ((and (string= kwop "|")
		(looking-at "|[^|]")
		(caml-in-indentation-p)) kwop)
	  ((string= kwop "|") (caml-find-|-match))
	  ((and (string= kwop "=") (or (looking-at "=[ \t]*\\((\\*\\|$\\)")
				       (caml-false-=-p)
				       (not (string= (save-excursion
						       (caml-find-=-match))
						     "type"))))
	   (caml-find-|-match))
	  ((string= kwop "parse")
	   (if (and (string-match "\\.mll" (buffer-name))
		    (save-excursion
		      (string= (caml-find-meaningful-word) "=")))
	       kwop (caml-find-|-match)))
	  (t kwop))))

(defconst caml-find-->-match-regexp
  (caml-make-find-kwop-regexp "\\<\\(with\\|fun\\(ction\\)?\\|parser\\)\\>\\|[|;]"))
(defun caml-find-->-match ()
  (let ((kwop (caml-find-kwop caml-find-->-match-regexp "\\<with\\>")))
    (if (and (string= kwop "|") (char-equal (char-before) ?|))
	(prog2 (forward-char -1) (caml-find-->-match))
      kwop)))

(defconst caml-find-semi-colon-match-regexp
  (caml-make-find-kwop-regexp ";[ \t]*\\((\\*\\|$\\)\\|->\\|\\<\\(let\\|method\\|with\\)\\>"))
(defun caml-find-semi-colon-match (&optional leading-semi-colon)
  (caml-find-kwop caml-find-semi-colon-match-regexp
			 "\\<\\(in\\|end\\|and\\|do\\|with\\)\\>")
  ;; we don't need to find the keyword matching `and' since we know it's `let'!
  (cond
   ((looking-at ";[ \t]*\\((\\*\\|$\\)")
    (forward-line 1)
    (while (or (caml-in-comment-p)
	       (looking-at "^[ \t]*\\((\\*\\|$\\)"))
      (forward-line 1))
    (back-to-indentation)
    (current-column))
   ((and leading-semi-colon
	 (looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*[^ \t\n]")
	 (not (looking-at "[[{(][|<]?[ \t]*\\((\\*\\|$\\)")))
    (current-column))
   ((looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*\\((\\*\\|$\\)")
    (caml-back-to-paren-or-indentation t)
    (+ (current-column) caml-default-indent))
   ((looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*[^ \t\n]")
    (caml-search-forward-paren)
    (current-column))
   ((looking-at "\\<method\\>[ \t]*\\((\\*\\|$\\)")
    (caml-back-to-paren-or-indentation)
    (+ (current-column) caml-method-indent))
   ((looking-at "\\<begin\\>[ \t]*\\((\\*\\|$\\)")
    (caml-back-to-paren-or-indentation t)
    (+ (current-column) caml-begin-indent))
   ((looking-at "->")
    (if (save-excursion
	  (caml-find-->-match)
	  (looking-at "\\<\\(with\\|fun\\(ction\\)?\\|parser\\)\\>\\||"))
	(prog2
	    (caml-back-to-paren-or-indentation)
	    (+ (current-column) caml-default-indent))
      (caml-find-semi-colon-match)))
   ((looking-at "\\<end\\>")
    (caml-find-match)
    (caml-find-semi-colon-match))
   ((looking-at "\\<in\\>")
    (caml-find-in-match)
    (caml-back-to-paren-or-indentation)
    (+ (current-column) caml-in-indent))
   (t (caml-back-to-paren-or-indentation t)
      (+ (current-column) caml-default-indent))))

(defconst caml-find-phrase-indentation-regexp
  (caml-make-find-kwop-regexp (concat caml-governing-phrase-regexp
					"\\|\\<and\\>")))
(defconst caml-find-phrase-indentation-regexp-pb
  (concat caml-find-phrase-indentation-regexp "\\|;;"))
(defconst caml-find-phrase-indentation-class-regexp
  (concat caml-matching-keyword-regexp "\\|\\<class\\>"))
(defun caml-find-phrase-indentation (&optional phrase-break)
  (if (and (looking-at "\\<\\(type\\|module\\)\\>") (> (point) (point-min))
	   (save-excursion
	     (caml-find-meaningful-word)
	     (looking-at "module\\|with\\|and\\|let")))
      (prog2
	  (caml-find-meaningful-word)
	  (+ (current-column) caml-default-indent))
    (let ((looking-at-and (looking-at "\\<and\\>"))
	  (kwop (caml-find-kwop
		 (if phrase-break
		     caml-find-phrase-indentation-regexp-pb
		   caml-find-phrase-indentation-regexp)
		 "\\<\\(end\\|and\\|with\\|in\\)\\>"))
	  (tmpkwop nil) (curr nil)
	  (old-point (save-excursion (beginning-of-line) (point))))
      (if (and kwop (string= kwop "and"))
	  (setq kwop (caml-find-and-match)))
      (if (not kwop) (current-column)
	(cond
	 ((string= kwop "end")
	  (if (not (save-excursion
		     (setq tmpkwop (caml-find-match))
		     (setq curr (point))
		     (string= tmpkwop "object")))
	      (prog2
		  (caml-find-match)
		  (caml-find-phrase-indentation phrase-break))
	    (caml-find-kwop caml-find-phrase-indentation-class-regexp)
	    (current-column)))
	 ((and (string= kwop "with")
	       (not (save-excursion
		      (setq tmpkwop (caml-find-with-match))
		      (setq curr (point))
		      (string= tmpkwop "module"))))
	  (goto-char curr)
	  (caml-find-phrase-indentation phrase-break))
	 ((and (string= kwop "in")
	       (not (save-excursion
		      (setq tmpkwop (caml-find-in-match))
		      (if (string= tmpkwop "and")
			  (setq tmpkwop (caml-find-and-match)))
		      (setq curr (point))
		      (and (string= tmpkwop "let")
			   (not (caml-looking-at-expression-let))))))
	  (goto-char curr)
	  (caml-find-phrase-indentation phrase-break))
	 ((caml-at-phrase-break-p)
	  (end-of-line)
	  (caml-skip-blank-and-comments)
	  (current-column))
	 ((string= kwop "let")
	  (if (caml-looking-at-expression-let)
	      (caml-find-phrase-indentation phrase-break)
	    (current-column)))
	 ((string= kwop "with")
	  (current-column))
	 ((string= kwop "end")
	  (current-column))
	 ((string= kwop "in")
	  (caml-find-in-match)
	  (current-column))
	 ((string= kwop "class")
	  (caml-back-to-paren-or-indentation)
	  (current-column))
	 ((looking-at "\\<\\(object\\|s\\(ig\\|truct\\)\\)\\>")
	  (caml-back-to-paren-or-indentation t)
	  (+ (caml-assoc-indent kwop) (current-column)))
	 ((or (string= kwop "type") (string= kwop "module"))
	  (if (or (caml-looking-at-false-type)
		  (caml-looking-at-false-module))
	      (if looking-at-and (current-column)
		(caml-find-meaningful-word)
		(if (looking-at "\\<and\\>")
		    (prog2
			(caml-find-and-match)
			(caml-find-phrase-indentation phrase-break))
		  (current-column)))
	    (current-column)))
	 ((looking-at
	   "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*\\((\\*\\|$\\)")
	  (caml-back-to-paren-or-indentation)
	  (+ (current-column) caml-default-indent))
	 ((looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*[^ \t\n]")
	  (caml-search-forward-paren)
	  (current-column))
	 ((string= kwop "open") ; compatibility with camllight `#open'
	  (caml-back-to-paren-or-indentation) (current-column))
	 (t (current-column)))))))

(defconst caml-back-to-paren-or-indentation-regexp
  "[][(){}]\\|\\*)\\|^[ \t]*\\(.\\|\n\\)")
(defconst caml-back-to-paren-or-indentation-in-regexp
  (concat "\\<in\\>\\|" caml-back-to-paren-or-indentation-regexp))
(defconst caml-back-to-paren-or-indentation-lazy-regexp
  "[])}]\\|\\*)\\|^[ \t]*\\(.\\|\n\\)")
(defconst caml-back-to-paren-or-indentation-lazy-in-regexp
  (concat "\\<in\\>\\|" caml-back-to-paren-or-indentation-regexp))
(defun caml-back-to-paren-or-indentation (&optional forward-in)
  "Searches backwards for the first open paren in line, or skip to indentation.
Returns t iff skipped to indentation."
  (if (or (bolp) (caml-in-indentation-p)) (prog2 (back-to-indentation) t)
    (let ((kwop (caml-find-kwop
		 (if caml-lazy-paren
		     (if forward-in
			 caml-back-to-paren-or-indentation-lazy-in-regexp
		       caml-back-to-paren-or-indentation-lazy-regexp)
		   (if forward-in
		       caml-back-to-paren-or-indentation-in-regexp
		     caml-back-to-paren-or-indentation-regexp))
		 "\\<and\\|with\\|in\\>"))
	  (retval))
      (if (string= kwop "with")
	  (let ((with-point (point)))
	    (setq kwop (caml-find-with-match))
	    (if (or (string= kwop "match") (string= kwop "try"))
		(caml-find-kwop
		 caml-back-to-paren-or-indentation-regexp
		 "\\<and\\>")
	      (setq kwop "with") (goto-char with-point))))
      (setq retval
	    (cond
	     ((string= kwop "with") nil)
	     ((string= kwop "in") (caml-in-indentation-p))
	     ((looking-at "[[{(]") (caml-search-forward-paren) nil)
	     (t (back-to-indentation) t)))
      (cond
       ((looking-at "|[^|]")
	(prog2 (re-search-forward "|[^|][ \t]*") nil))
       ((and forward-in (string= kwop "in"))
	(caml-find-in-match)
	(caml-back-to-paren-or-indentation forward-in)
	(if (looking-at "\\<\\(let\\|and\\)\\>")
	    (forward-char caml-in-indent)) nil)
       (t retval)))))

(defun caml-search-forward-paren ()
  (if caml-lazy-paren (caml-back-to-paren-or-indentation)
    (re-search-forward "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*")))

(defun caml-add-default-indent (leading-operator)
  (if leading-operator 0 caml-default-indent))

(defconst caml-compute-argument-indent-regexp
  (caml-make-find-kwop-regexp caml-kwop-regexp))
(defun caml-compute-argument-indent (leading-operator)
  (let ((old-point (save-excursion (beginning-of-line) (point)))
	(match-end-point) (kwop))
    (setq kwop (caml-find-kwop caml-compute-argument-indent-regexp
				 caml-keyword-regexp))
    (setq match-end-point (+ (point) (length kwop))) ; match-end is invalid !
    (if (and (if caml-lazy-= (looking-at "[[{(]") (looking-at "[[{(=]"))
	     (not (looking-at "=[ \t]*\\((\\*.*\\)?$")))
	(prog2
	    (if (and (not caml-lazy-=)
		     (not (caml-if-when-=))
		     (looking-at "=")) (re-search-forward "=[ \t]*")
	      (caml-search-forward-paren))
	    (+ (caml-add-default-indent leading-operator)
	       (current-column)))
      (if (<= old-point (point))
	  (+ (caml-add-default-indent leading-operator) (current-column))
	(forward-line 1)
	(beginning-of-line)
	(while (or (caml-in-comment-p)
		   (looking-at "^[ \t]*\\((\\*.*\\)?$"))
	  (forward-line 1))
	(caml-back-to-paren-or-indentation)
	(if (save-excursion (goto-char match-end-point)
			    (looking-at "[ \t]*\\((\\*.*\\)?$"))
	    (+ (caml-add-default-indent leading-operator)
	       (current-column))
	  (current-column))))))

(defconst caml-compute-normal-indent-regexp
  (concat caml-compute-argument-indent-regexp "\\|^[ \t]*[^ \t\n]"))
(defun caml-compute-normal-indent ()
  (let ((leading-operator (looking-at caml-operator-regexp)))
    (beginning-of-line)
    (save-excursion
      (caml-find-meaningful-word)
      (if (looking-at caml-operator-regexp) (setq leading-operator t)))
    (save-excursion
      (let ((kwop (caml-find-kwop (if leading-operator
					caml-compute-argument-indent-regexp
				      caml-compute-normal-indent-regexp)
				    caml-keyword-regexp))
	    (leading-special 0))
	(if (string= kwop "and") (setq kwop (caml-find-and-match)))
	(while (and (string= kwop "=") (caml-false-=-p))
	  (setq kwop (caml-find-kwop caml-compute-normal-indent-regexp
				       caml-keyword-regexp))
	  (if (string= kwop "and") (setq kwop (caml-find-and-match))))
	(if (not kwop) (current-column)
	  (cond
	   ((caml-at-phrase-break-p)
	    (caml-find-phrase-indentation t))
	   ((and (string= kwop "|") (not  (char-equal ?\[ (preceding-char))))
	    (caml-backward-char)
	    (caml-back-to-paren-or-indentation)
	    (+ (current-column) caml-default-indent
	       (caml-add-default-indent leading-operator)))
	   ((or (looking-at "[[{(]")
		(and (looking-at "[<|]")
		     (char-equal ?\[ (preceding-char))
		     (prog2 (caml-backward-char) t))
		(and (looking-at "<")
		     (char-equal ?\{ (preceding-char))
		     (prog2 (caml-backward-char) t)))
	    (if (looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*\\((\\*\\|$\\)")
		(prog2
		    (caml-back-to-paren-or-indentation t)
		    (+ caml-default-indent
		       (current-column))) ; parens do not operate
	      (caml-search-forward-paren)
	      (+ (caml-add-default-indent leading-operator)
		 (current-column))))
	   ((and (looking-at "->")
		 (save-excursion
		   (string= (caml-find-->-match) "|")))
	    (caml-find-->-match)
	    (re-search-forward "|[ \t]*")
	    (+ (current-column) caml-default-indent))
	   ((looking-at caml-keyword-regexp)
	    (cond ((or (string= kwop ",") (string= kwop ";"))
		   (if (looking-at ";[ \t]*\\((\\*\\|$\\)")
		       (caml-find-semi-colon-match)
		     (if (looking-at ",[ \t]*\\((\\*\\|$\\)")
			 (progn
			   (caml-back-to-paren-or-indentation t)
			   (current-column))
		       (caml-back-to-paren-or-indentation t)
		       (+ (current-column) caml-default-indent))))
		  ((and (looking-at "\\<\\(in\\|begin\\|do\\)\\>\\|->")
			(not (looking-at
			      "\\([a-z]+\\|->\\)[ \t]*\\((\\*\\|$\\)")))
		   (if (string= kwop "in")
		       (re-search-forward "\\<in\\>[ \t]*")
		     (caml-back-to-paren-or-indentation t))
		   (+ (current-column)
		      (caml-add-default-indent leading-operator)
		      (if (string= kwop "in") 0 ; aligned, do not indent
			(caml-assoc-indent kwop))))
		  ((string= kwop "with")
		   (if (save-excursion
			 (let ((tmpkwop (caml-find-with-match)))
			   (or (string= tmpkwop "module")
			       (string= tmpkwop "{"))))
		       (prog2
			   (caml-back-to-paren-or-indentation)
			   (+ (current-column) caml-default-indent))
		     (caml-back-to-paren-or-indentation)
		     (+ (current-column)
			(caml-assoc-indent kwop
					     (looking-at "\\<let\\>")))))
		  ((string= kwop "in")
		   (caml-find-in-match)
		   (caml-back-to-paren-or-indentation)
		   (+ (current-column) caml-in-indent))
		  (t (caml-back-to-paren-or-indentation t)
		     (+ (current-column)
			(caml-assoc-indent kwop
					     (looking-at "\\<let\\>"))))))
	   ((looking-at "=")
	    (if (or caml-lazy-= (caml-if-when-=)
		    (looking-at "=[ \t]*\\((\\*\\|$\\)")) ; not perfect...
		(let ((current-column-module-type nil))
		  (+
		   (progn
		     (caml-find-=-match)
		     (save-excursion
		       (if (looking-at "\\<and\\>") (caml-find-and-match))
		       (cond
			((looking-at "\\<type\\>")
			 (caml-find-meaningful-word)
			 (if (looking-at "\\<module\\>")
			     (progn
			       (setq current-column-module-type
				     (current-column))
			       caml-default-indent)
			   (if (looking-at "\\<\\(with\\|and\\)\\>")
			       (progn
				 (setq current-column-module-type
				       (current-column))
				 caml-default-indent)
			     (re-search-forward "\\<type\\>")
			     (beginning-of-line)
			     (+ caml-type-indent
				caml-|-extra-unindent))))
			((looking-at
			  "\\<\\(let\\|m\\(ethod\\|odule\\)\\|class\\|when\\|\\|for\\|if\\)\\>")
			 (let ((matched-string (caml-match-string 0)))
			   (caml-back-to-paren-or-indentation t)
			   (setq current-column-module-type (current-column))
			   (caml-assoc-indent matched-string)))
			((looking-at
			  "\\<object\\>")
			 (caml-back-to-paren-or-indentation t)
			 (setq current-column-module-type (current-column))
			 (+ (caml-assoc-indent "object")
			    caml-default-indent))
			(t (caml-search-forward-paren)
			   (setq current-column-module-type (current-column))
			   caml-default-indent))))
		   (if current-column-module-type
		       current-column-module-type
		     (current-column))))
	      (+ (caml-add-default-indent leading-operator)
		 (prog2
		     (re-search-forward "=[ \t]*")
		     (current-column)))))
	   (nil 0)
	   (t (caml-compute-argument-indent leading-operator))))))))

(defun caml-looking-at-expression-let ()
  (save-excursion
    (and (caml-find-meaningful-word)
	 (not (caml-at-phrase-break-p))
	 (or (looking-at "[[({;=]\\|\\<\\(begin\\|i[fn]\\|do\\|t\\(ry\\|hen\\)\\|else\\|match\\|wh\\(ile\\|en\\)\\)\\>")
	     (looking-at caml-operator-regexp)))))

(defun caml-looking-at-false-module ()
  (save-excursion (caml-find-meaningful-word)
		  (looking-at "\\<\\(let\\|with\\|and\\)\\>")))

(defun caml-looking-at-false-sig-struct ()
  (save-excursion (caml-find-module)
		  (looking-at "\\<module\\>")))

(defun caml-looking-at-false-type ()
  (save-excursion (caml-find-meaningful-word)
		  (looking-at "\\<\\(class\\|with\\|module\\|and\\)\\>")))

(defun caml-looking-at-in-let ()
  (save-excursion (string= (caml-find-meaningful-word) "in")))

(defconst caml-find-module-regexp
  (caml-make-find-kwop-regexp "\\<module\\>"))
(defun caml-find-module ()
  (caml-find-kwop caml-find-module-regexp))

(defun caml-indent-command ()
  "Indent the current line in Caml mode.

Compute new indentation based on Caml syntax."
  (interactive "*")
  (save-excursion
    (setq case-fold-search nil)
    (back-to-indentation)
    (indent-line-to (caml-compute-indent)))
  (if (caml-in-indentation-p) (back-to-indentation)))


(defun caml-compute-indent ()
  (save-excursion
    (cond
     ((caml-in-comment-p)
      (cond
       ((looking-at "(\\*")
	(if caml-indent-leading-comments
	    (if (save-excursion (forward-line -1)
				(beginning-of-line)
				(not (looking-at "[ \t]*$")))
		(caml-compute-normal-indent)
	      (save-excursion
		(caml-skip-blank-and-comments)
		(if (looking-at "$") 0 (caml-compute-indent))))
	  (current-column)))
       ((looking-at "\\*\\**)")
	(caml-beginning-of-literal-or-comment-fast)
	(if caml-support-leading-star-comments
	    (+ (current-column)
	       (if (save-excursion
		     (forward-line 1)
		     (back-to-indentation)
		     (looking-at "*")) 1
		 caml-comment-end-extra-indent))
	  (+ (current-column) caml-comment-end-extra-indent)))
       (caml-indent-comments
	(let ((star (and caml-support-leading-star-comments
			 (looking-at "\\*"))))
	  (caml-beginning-of-literal-or-comment-fast)
	  (if star (re-search-forward "(") (re-search-forward "(\\*[ \t]*"))
	  (current-column)))))
     ((caml-in-literal-p)
      (current-column))
     ((looking-at "\\<let\\>")
      (if (caml-looking-at-expression-let)
	  (if (caml-looking-at-in-let)
	      (progn
		(caml-find-meaningful-word)
		(caml-find-in-match)
		(caml-back-to-paren-or-indentation)
		(current-column))
	    (caml-compute-normal-indent))
	(caml-find-phrase-indentation)))
     ((looking-at caml-governing-phrase-regexp-with-break)
      (caml-find-phrase-indentation))
     ((and caml-sig-struct-align (looking-at "\\<\\(sig\\|struct\\)\\>"))
      (if (string= (caml-find-module) "module") (current-column)
	(caml-back-to-paren-or-indentation)
	(+ caml-default-indent (current-column))))
     ((looking-at ";") (caml-find-semi-colon-match t))
     ((looking-at caml-leading-kwop-regexp)
      (let ((kwop (caml-match-string 0)))
	(if (looking-at "[#%]\\|;;") 0
	  (let* ((old-point (point))
		 (paren-match-p (looking-at "[|>]?[]})]"))
		 (need-not-back-kwop (string= kwop "and"))
		 (real-| (looking-at "|\\([^|]\\|$\\)"))
		 (matching-kwop
		  (if (string= kwop "and") (caml-find-and-match t)
		    (funcall (cdr (assoc kwop caml-leading-kwop-alist)))))
		 (match-|-keyword-p
		  (looking-at caml-match-|-keyword-regexp)))
	    (cond
	     ((and (looking-at "\\(\\[|?\\|{<?\\|(\\)[ \t]*[^ \t\n]")
		   (not (looking-at "[[{(][|<]?[ \t]*\\((\\*\\|$\\)")))
	      (if (and (string= kwop "|") real-|) (current-column)
		(if (not paren-match-p) (caml-search-forward-paren))
		(if caml-lazy-paren (caml-back-to-paren-or-indentation))
		(current-column)))
	     ((and (string= kwop "|") real-|)
	      (cond
	       ((string= matching-kwop "|")
		(if (not need-not-back-kwop)
		    (caml-back-to-paren-or-indentation))
		(current-column))
	       ((and (string= matching-kwop "=")
		     (not (caml-false-=-p)))
		(re-search-forward "=[ \t]*")
		(current-column))
	       (match-|-keyword-p
		(if (not need-not-back-kwop)
		    (caml-back-to-paren-or-indentation))
		(- (+ (caml-assoc-indent
		       matching-kwop (looking-at "\\<let\\>"))
		      (current-column))
		   (if (string= matching-kwop "type") 0
		     caml-|-extra-unindent)))
	       (t (goto-char old-point)
		  (caml-compute-normal-indent))))
	     ((and (string= kwop "|") (not real-|))
	      (goto-char old-point)
	      (caml-compute-normal-indent))
	     ((not need-not-back-kwop)
	      (caml-back-to-paren-or-indentation (not (string= kwop "in")))
	      (current-column))
	     (t (current-column)))))))
     (t (caml-compute-normal-indent)))))

(defun caml-split-string ()
  "Called whenever a line is broken inside a Caml string literal."
  (insert-before-markers "\" ^\"")
  (caml-backward-char))

(defadvice newline-and-indent (around
			       caml-newline-and-indent
			       activate)
  "Handle multi-line strings in Caml mode."
    (let ((hooked (and (eq major-mode 'caml-mode) (caml-in-literal-p)))
	  (split-mark))
      (if (not hooked) nil
	(setq split-mark (set-marker (make-marker) (point)))
	(caml-split-string))
      ad-do-it
      (if (not hooked) nil
	(goto-char split-mark)
	(set-marker split-mark nil))))

(defun caml-electric ()
  "If inserting a |, ) operator at beginning of line, reindent the line."
  (interactive "*")
  (let ((electric (and caml-electric-indent
		       (caml-in-indentation-p)
		       (not (caml-in-literal-or-comment-p)))))
    (self-insert-command 1)
    (if (and electric
	     (not (and (char-equal ?| (preceding-char))
		       (save-excursion
			 (caml-backward-char)
			 (caml-find-|-match)
			 (not (looking-at caml-match-|-keyword-regexp))))))
	(caml-indent-command))))

(defun caml-electric-rc ()
  "If inserting a } operator at beginning of line, reindent the line.

Reindent also if } is inserted after a > operator at beginning of line.
Also, if the matching { is followed by a < and this } is not preceded
by >, insert one >."
  (interactive "*")
  (let* ((prec (preceding-char))
	 (look-bra (and caml-electric-close-vector
			(not (caml-in-literal-or-comment-p))
			(not (char-equal ?> prec))))
	 (electric (and caml-electric-indent
			(or (caml-in-indentation-p)
			    (and (char-equal ?> prec)
				 (save-excursion (caml-backward-char)
						 (caml-in-indentation-p))))
			(not (caml-in-literal-or-comment-p)))))
    (self-insert-command 1)
    (if look-bra
	(save-excursion
	  (let ((inserted-char
		 (save-excursion
		   (caml-backward-char)
		   (caml-backward-up-list)
		   (cond ((looking-at "{<") ">")
			 (t "")))))
	    (caml-backward-char)
	    (insert inserted-char))))
    (if electric (caml-indent-command))))

(defun caml-electric-rb ()
  "If inserting a ] operator at beginning of line, reindent the line.

Reindent also if ] is inserted after a | or > operator at beginning of line.
Also, if the matching [ is followed by a | (resp. <) and this ] is not
preceded by | (resp. >), insert one | (resp. >)."
  (interactive "*")
  (let* ((prec (preceding-char))
	 (look-|-or-bra (and caml-electric-close-vector
			     (not (caml-in-literal-or-comment-p))
			     (not (or (and (char-equal ?| prec)
					   (not (char-equal
						 (save-excursion
						   (caml-backward-char)
						   (preceding-char)) ?\[)))
				      (char-equal ?> prec)))))
	 (electric (and caml-electric-indent
			(or (caml-in-indentation-p)
			    (and (or (char-equal ?| prec) (char-equal ?> prec))
				 (save-excursion (caml-backward-char)
						 (caml-in-indentation-p))))
			(not (caml-in-literal-or-comment-p)))))
    (self-insert-command 1)
    (if look-|-or-bra
	(save-excursion
	  (let ((inserted-char
		 (save-excursion
		   (caml-backward-char)
		   (caml-backward-up-list)
		   (cond ((looking-at "\\[|") "|")
			 ((looking-at "\\[<") ">")
			 (t "")))))
	    (caml-backward-char)
	    (insert inserted-char))))
    (if electric (caml-indent-command))))

(defun caml-abbrev-hook ()
  "If inserting a leading keyword at beginning of line, reindent the line."
  (if (not (caml-in-literal-or-comment-p))
      (let* ((bol (save-excursion (beginning-of-line) (point)))
	     (kw (save-excursion
		   (and (re-search-backward "^[ \t]*\\(\\w+\\)\\=" bol t)
			(caml-match-string 1)))))
	(if kw (progn
		   (insert " ")
		   (caml-indent-command)
		   (backward-delete-char-untabify 1))))))

(defun caml-skip-to-end-of-phrase ()
  (let ((old-point (point)))
    (if (and (string= (caml-find-meaningful-word) ";")
	     (char-equal (preceding-char) ?\;))
	(setq old-point (1- (point))))
    (goto-char old-point)
    (let ((kwop (caml-find-meaningful-word)))
      (goto-char (+ (point) (length kwop))))))

(defun caml-skip-blank-and-comments ()
  (skip-chars-forward " \t\n")
  (while (and (< (point) (point-max)) (caml-in-comment-p)
	      (re-search-forward "\\*)" (point-max) t))
    (skip-chars-forward " \t\n")))

(defun caml-skip-back-blank-and-comments ()
  (skip-chars-backward " \t\n")
  (while (save-excursion (caml-backward-char)
			 (and (> (point) (point-min)) (caml-in-comment-p)))
    (caml-backward-char)
    (caml-beginning-of-literal-or-comment) (skip-chars-backward " \t\n")))

(defconst caml-beginning-phrase-regexp
  "^#[ \t]*[a-z][_a-z]*\\>\\|\\<\\(end\\|val\\|value\\|type\\|module\\|sig\\|struct\\|class\\|exception\\|open\\|let\\)\\>\\|;;"
  "Regexp matching caml phrase delimitors.")
(defun caml-find-phrase-beginning ()
  "Find `real' phrase beginning and returns point."
  (beginning-of-line)
  (caml-skip-blank-and-comments)
  (end-of-line)
  (caml-skip-to-end-of-phrase)
  (let ((old-point (point)))
    (caml-find-kwop caml-beginning-phrase-regexp)
    (while (and (> (point) (point-min)) (< (point) old-point)
		(or (not (looking-at caml-beginning-phrase-regexp))
		    (and (looking-at "\\<let\\>")
			 (caml-looking-at-expression-let))
		    (and (looking-at "\\<module\\>")
			 (caml-looking-at-false-module))
		    (and (looking-at "\\<\\(sig\\|struct\\)\\>")
			 (caml-looking-at-false-sig-struct))
		    (and (looking-at "\\<type\\>")
			 (caml-looking-at-false-type))))
      (if (looking-at "\\<end\\>")
	  (caml-find-match)
	(if (not (bolp)) (caml-backward-char))
	(setq old-point (point))
	(caml-find-kwop caml-beginning-phrase-regexp)))
    (if (caml-at-phrase-break-p)
	(prog2 (end-of-line) (caml-skip-blank-and-comments)))
    (back-to-indentation)
    (point)))

(defun caml-search-forward-end ()
  (re-search-forward "\\<\\(end\\|begin\\)\\>" (point-max) t)
  (if (string= (caml-match-string 0) "\\<begin\\>") (caml-search-forward-end)))

(defconst caml-inside-block-regexp
  (concat caml-matching-keyword-regexp
	  "\\|\\<\\(class\\|sig\\|struct\\)\\>"))
(defun caml-inside-block-find-kwop ()
  (let ((kwop (caml-find-kwop caml-inside-block-regexp
				"\\<\\(and\\|end\\)\\>")))
    (if (string= kwop "and") (setq kwop (caml-find-and-match)))
    (if (string= kwop "end")
	(progn
	  (caml-find-match)
	  (caml-find-kwop caml-inside-block-regexp)
	  (caml-inside-block-find-kwop))
      kwop)))
(defun caml-inside-block-p ()
  (let ((begin) (end) (and-end) (kwop t))
    (save-excursion
      (if (looking-at "\\<and\\>")
	  (caml-find-and-match))
      (if (not (looking-at "\\<\\(class\\|sig\\|struct\\)\\>"))
	  (while (and (setq kwop (caml-inside-block-find-kwop))
		      (not (looking-at "\\<\\(class\\|sig\\|struct\\)\\>")))))
      (if (not kwop) ()
	(setq begin (point))
	(while (and (caml-search-forward-end)
		    (save-excursion
		      (caml-backward-char 3)
		      (caml-find-match)
		      (if (looking-at "\\<object\\>")
			  (caml-inside-block-find-kwop))
		      (> (point) begin))))
	(caml-backward-char 3)
	(if (not (looking-at "\\<end\\>")) ()
	  (caml-forward-char 3)
	  (setq end (point))
	  (setq and-end (point))
	  (caml-skip-blank-and-comments)
	  (while (looking-at "\\<and\\>")
	    (setq and-end (point))
	    (while (and (caml-search-forward-end)
			(save-excursion
			  (caml-backward-char 3)
			  (caml-find-match)
			  (if (looking-at "\\<object\\>")
			      (caml-inside-block-find-kwop))
			  (> (point) and-end))))
	    (caml-backward-char 3)
	    (if (not (looking-at "\\<end\\>")) ()
	      (caml-forward-char 3)
	      (setq and-end (point))
	      (caml-skip-blank-and-comments)))
	  (list begin end and-end))))))
	    
(defun caml-discover-phrase (&optional quiet)
  (end-of-line)
  (let ((end (point)))
    (caml-find-phrase-beginning)
    (if (> (point) end) (setq end (point)))
    (save-excursion
      (let ((begin (point)) (cpt 0) (lines-left 0) (stop)
	    (inside-block (caml-inside-block-p))
	    (looking-block (looking-at "\\<class\\>")))
	(if (and looking-block inside-block)
	    (progn
	      (setq begin (nth 0 inside-block))
	      (setq end (nth 2 inside-block))
	      (goto-char end))
	  (if inside-block
	      (progn
		(setq stop (save-excursion (goto-char (nth 1 inside-block))
					   (beginning-of-line) (point)))
		(if (< stop end) (setq stop (point-max))))
	    (setq stop (point-max)))
	  (save-restriction
	    (goto-char end)
	    (while (and (= lines-left 0)
			(or (not inside-block) (< (point) stop))
			(<= (save-excursion
			      (caml-find-phrase-beginning)) end))
	      (if (not quiet)
		  (prog2
		      (setq cpt (1+ cpt))
		      (if (= 8 cpt)
			  (message "Looking for enclosing phrase..."))))
	      (setq end (point))
	      (caml-skip-to-end-of-phrase)
	      (beginning-of-line)
	      (narrow-to-region (point) (point-max))
	      (goto-char end)
	      (setq lines-left (forward-line 1)))))
	(if (>= cpt 8) (message "Looking for enclosing phrase... done."))
	(save-excursion (caml-skip-blank-and-comments) (setq end (point)))
	(caml-skip-back-blank-and-comments)
	(list begin (point) end)))))

(defun caml-mark-phrase ()
  "Put mark at end of this Caml phrase, point at beginning.
The Caml phrase is the phrase just before the point."
  (interactive)
  (let ((pair (caml-discover-phrase)))
    (goto-char (nth 1 pair)) (push-mark (nth 0 pair) t t)))

(defun caml-next-phrase (&optional quiet)
  "Skip to the beginning of the next phrase."
  (interactive "i")
  (goto-char (save-excursion (nth 2 (caml-discover-phrase quiet))))
  (if (looking-at "\\<end\\>") (caml-next-phrase quiet))
  (if (looking-at ";;")
      (progn
	(forward-char 2)
	(caml-skip-blank-and-comments))))

(defun caml-previous-phrase ()
  "Skip to the beginning of the previous phrase."
  (interactive)
  (beginning-of-line)
  (caml-skip-to-end-of-phrase)
  (caml-discover-phrase))

(defun caml-indent-phrase ()
  "If inside a comment call `fill-paragraph-or-region'; Otherwise
indent all lines in the current phrase."
  (interactive)
  (save-excursion
    (if (and (caml-in-comment-p)
	     (save-excursion (beginning-of-line) (caml-in-comment-p)))
	(let ((begpoint (save-excursion
			  (caml-beginning-of-literal-or-comment) (point)))
	      (endpoint (save-excursion
			  (while (caml-in-comment-p)
			    (re-search-forward "*)")) (point))))
	  (fill-region begpoint endpoint))
      (let ((pair (caml-discover-phrase)))
	(indent-region (nth 0 pair) (nth 1 pair) nil)))))

;; Auxiliary function (from J. Garrigue)
(defun caml-find-alternate-file ()
  "Switch Implementation/Interface."
  (interactive)
  (let ((name (buffer-file-name)))
    (if (string-match "^\\(.*\\)\\.\\(ml\\|mli\\)$" name)
	(find-file (concat (caml-match-string 1 name)
			   (if (string= "ml" (caml-match-string 2 name))
			       ".mli" ".ml"))))))

(defun caml-insert-begin-form ()
  "Inserts a nicely formatted begin-end form, leaving a mark after end."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "begin\n\nend\n")
    (end-of-line)
    (indent-region old (point) nil)
    (push-mark)
    (forward-line -2)
    (caml-indent-command)))

(defun caml-insert-for-form ()
  "Inserts a nicely formatted for-to-done form, leaving a mark after done."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "for  do\n\ndone\n")
    (end-of-line)
    (indent-region old (point) nil)
    (push-mark)
    (forward-line -2)
    (caml-indent-command)
    (beginning-of-line 1)
    (backward-char 4)))

(defun caml-insert-while-form ()
  "Inserts a nicely formatted for-to-done form, leaving a mark after done."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "while  do\n\ndone\n")
    (end-of-line)
    (indent-region old (point) nil)
    (push-mark)
    (forward-line -2)
    (caml-indent-command)
    (beginning-of-line 1)
    (backward-char 4)))

(defun caml-insert-if-form ()
  "Inserts a nicely formatted if-then-else form, leaving a mark after else."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "if\n\nthen\n\nelse\n")
    (end-of-line)
    (indent-region old (point) nil)
    (caml-indent-command)
    (push-mark)
    (forward-line -2)
    (caml-indent-command)
    (forward-line -2)
    (caml-indent-command)))

(defun caml-insert-match-form ()
  "Inserts a nicely formatted math-with form, leaving a mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "match\n\nwith\n")
    (end-of-line)
    (indent-region old (point) nil)
    (caml-indent-command)
    (push-mark)
    (forward-line -2)
    (caml-indent-command)))

(defun caml-insert-let-form ()
  "Inserts a nicely formatted let-in form, leaving a mark after in."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "let  in\n")
    (end-of-line)
    (indent-region old (point) nil)
    (caml-indent-command)
    (push-mark)
    (beginning-of-line)
    (backward-char 4)
    (caml-indent-command)))

(defun caml-insert-try-form ()
  "Inserts a nicely formatted try-with form, leaving a mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "try\n\nwith\n")
    (end-of-line)
    (indent-region old (point) nil)
    (caml-indent-command)
    (push-mark)
    (forward-line -2)
    (caml-indent-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Caml interactive mode

;; Augment Caml mode, so you can process Caml code in the source files.

(require 'comint)

(defvar caml-interactive-mode-map nil)
(if caml-interactive-mode-map nil
  (setq caml-interactive-mode-map
        (copy-keymap comint-mode-map)))
(define-key caml-interactive-mode-map "#" 'caml-electric)
(define-key caml-interactive-mode-map "%" 'caml-electric)
(define-key caml-interactive-mode-map "|" 'caml-electric)
(define-key caml-interactive-mode-map ")" 'caml-electric)
(define-key caml-interactive-mode-map "}" 'caml-electric-rc)
(define-key caml-interactive-mode-map "]" 'caml-electric-rb)
(define-key caml-interactive-mode-map "\t" 'caml-indent-command)
(define-key caml-interactive-mode-map "\C-c\C-i" 'caml-interrupt-caml)
(define-key caml-interactive-mode-map "\C-c\C-k" 'caml-kill-caml)
(define-key caml-interactive-mode-map "\C-c`"
  'caml-interactive-next-error-toplevel)
(define-key caml-interactive-mode-map "\C-m"
  'caml-interactive-send-input)
(define-key caml-interactive-mode-map "\C-j"
  'caml-interactive-send-input-or-indent)
(define-key caml-interactive-mode-map "\M-\C-m"
  'comint-send-input)
(if (functionp 'read-kbd-macro)
    (define-key caml-interactive-mode-map (read-kbd-macro "<kp-enter>")
      'comint-send-input))

(defconst caml-interactive-buffer-name "*caml-toplevel*")

(defconst caml-interactive-toplevel-error-regexp
  "Characters \\([0-9]+\\)-\\([0-9]+\\):"
  "Regular expression extracting the character numbers
from an error message produced by ocaml toplevel.")
(defvar caml-interactive-last-phrase-pos-in-source 0)
(defvar caml-interactive-last-phrase-pos-in-toplevel 0)

(defun caml-interactive-filter (text)
  (if (eq major-mode 'caml-interactive-mode)
      (save-excursion
	(if (< comint-last-input-end comint-last-input-start) ()
	  (if (and caml-with-xemacs caml-interactive-read-only-input)
	      (add-text-properties comint-last-input-start
				   comint-last-input-end
				   (list 'read-only t)))
	  (if (and (or caml-window-system caml-with-xemacs)
		   (featurep 'font-lock)
		   caml-interactive-input-font-lock)
	      (progn
		(font-lock-fontify-region comint-last-input-start
					  comint-last-input-end)
		(if (featurep 'sym-lock)
		    (sym-lock-make-symbols-atomic comint-last-input-start
						  comint-last-input-end))))
	  (if caml-interactive-output-font-lock
	      (save-excursion
		(add-text-properties
		 comint-last-input-end (point-max)
		 '(face caml-font-lock-interactive-output-face))))
	  (if caml-interactive-error-font-lock
	      (save-excursion
		(goto-char comint-last-input-end)
		(while (re-search-forward caml-interactive-error-regexp () t)
		  (add-text-properties
		   (match-beginning 1) (match-end 1)
		   '(face caml-font-lock-interactive-error-face))
		  (goto-char (match-beginning 1))
		  (if (re-search-forward
		       caml-interactive-toplevel-error-regexp () t)
		      (let ((beg (string-to-int (caml-match-string 1)))
			    (end (string-to-int (caml-match-string 2))))
			(add-text-properties
			  (+ caml-interactive-last-phrase-pos-in-toplevel
			     beg)
			  (+ caml-interactive-last-phrase-pos-in-toplevel
			     end)
			  '(face highlight)))))))))))


(defun caml-interactive-mode ()
  "Major mode for interacting with a Caml process.
Runs a Caml toplevel as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in Caml mode.

Special keys for Caml interactive mode:\\{caml-interactive-mode-map}"
  (interactive)
  (comint-mode)
  (if (not (eq (caml-install-font-lock) 'font-lock)) ()
    (add-hook 'comint-output-filter-functions 'caml-interactive-filter)
    (if (not (boundp 'after-change-functions)) ()
      (make-local-hook 'after-change-functions)
      (put 'after-change-functions 'permanent-local t)
      (remove-hook 'after-change-functions 'font-lock-after-change-function t))
    (if (not (boundp 'pre-idle-hook)) ()
      (make-local-hook 'pre-idle-hook)
      (put 'pre-idle-hook 'permanent-local t)
      (remove-hook 'pre-idle-hook 'font-lock-pre-idle-hook t)))
  (setq comint-prompt-regexp "^#")
  (setq major-mode 'caml-interactive-mode)
  (setq mode-name "Caml-Interactive")
  (setq comint-scroll-to-bottom-on-output t)
  (use-local-map caml-interactive-mode-map)
  (set-syntax-table caml-mode-syntax-table)
  (setq local-abbrev-table caml-mode-abbrev-table)

  (if caml-window-system (caml-interactive-build-menu))

  ;; hooks for caml-interactive-mode
  (run-hooks 'caml-interactive-mode-hook))

(defun caml-run-caml (&optional cmd)
  "Run a Caml toplevel process. I/O via buffer `*caml-toplevel*'."
  (interactive
   (list (if (not (comint-check-proc caml-interactive-buffer-name))
	     (read-from-minibuffer "Caml toplevel to run: "
				   caml-interactive-program))))
  (caml-run-process-if-needed cmd)
  (display-buffer caml-interactive-buffer-name))

(defun caml-run-process-if-needed (&optional cmd)
  (if (not cmd)
      (if (comint-check-proc caml-interactive-buffer-name)
	  (setq cmd caml-interactive-program)
	(setq cmd (read-from-minibuffer "Caml toplevel to run: "
					caml-interactive-program))))
  (setq caml-interactive-program cmd)
  (if (not (comint-check-proc caml-interactive-buffer-name))
      (let ((cmdlist (caml-args-to-list cmd))
            (process-connection-type nil))
	(set-buffer (apply (function make-comint) "caml-toplevel"
			   (car cmdlist) nil (cdr cmdlist)))
	(caml-interactive-mode)
	(sleep-for 1))))

(defun caml-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (caml-args-to-list (substring string (+ 1 where)
						 (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (caml-args-to-list (substring string pos
						 (length string)))))))))

(defun caml-interactive-end-of-phrase ()
  (save-excursion
    (end-of-line)
    (caml-find-meaningful-word)
    (caml-find-meaningful-word)
    (looking-at ";;")))

(defun caml-interactive-send-input ()
  "Process if the current line ends with `;;' then send the
current phrase else insert a newline."
  (interactive)
  (if (caml-interactive-end-of-phrase)
      (comint-send-input)
    (insert "\n")
    (message "Phrase must end with `;;' to be processed by Caml toplevel...")))

(defun caml-interactive-send-input-or-indent ()
  "Process if the current line ends with `;;' then send the
current phrase else insert a newline and indent."
  (interactive)
  (if (caml-interactive-end-of-phrase)
      (comint-send-input)
    (insert "\n")
    (caml-indent-command)
    (message "Phrase must end with `;;' to be processed by Caml toplevel...")))

(defun caml-eval-region (start end)
  "Eval the current region in the Caml toplevel."
  (interactive "r")
  (save-excursion (caml-run-process-if-needed))
  (comint-preinput-scroll-to-bottom)
  (setq caml-interactive-last-phrase-pos-in-source start)
  (save-excursion
    (let ((text (buffer-substring-no-properties start end)))
      (goto-char end)
      (if (progn (caml-find-meaningful-word) (caml-find-meaningful-word)
		 (looking-at ";;")) ()
	(setq text (concat text ";;")))
      (if (string= text "")
	  (message "Cannot send empty commands to Caml toplevel!")
	(set-buffer caml-interactive-buffer-name)
	(goto-char (point-max))
	(setq caml-interactive-last-phrase-pos-in-toplevel (point))
	(if caml-interactive-echo-phrase (insert text)
	  (comint-send-string caml-interactive-buffer-name text)
	  (comint-send-string caml-interactive-buffer-name "\n"))
	(comint-send-input)))
    (display-buffer caml-interactive-buffer-name)))

(defun caml-eval-phrase ()
  "Eval the surrounding Caml phrase in the Caml toplevel."
  (interactive)
  (let ((end))
    (save-excursion
      (let ((pair (caml-discover-phrase)))
	(setq end (nth 2 pair))
	(caml-eval-region (nth 0 pair) (nth 1 pair))))
    (if caml-skip-after-eval-phrase
	(goto-char end))))

(defun caml-eval-buffer ()
  "Send the buffer to the Caml Interactive process."
  (interactive)
  (caml-eval-region (point-min) (point-max)))

(defun caml-interactive-next-error-source ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (save-excursion
      (set-buffer caml-interactive-buffer-name)
      (goto-char caml-interactive-last-phrase-pos-in-toplevel)
      (setq error-pos
	    (re-search-forward caml-interactive-toplevel-error-regexp
			       (point-max) t))
      (if error-pos
	  (progn
	    (setq beg (string-to-int (caml-match-string 1))
		  end (string-to-int (caml-match-string 2))))))
    (if (not error-pos)
	(message "No syntax or typing error in last phrase.")
      (setq beg (+ caml-interactive-last-phrase-pos-in-source beg)
	    end (+ caml-interactive-last-phrase-pos-in-source end))
      (goto-char beg) (push-mark end t t))))

(defun caml-interactive-next-error-toplevel ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (save-excursion
      (goto-char caml-interactive-last-phrase-pos-in-toplevel)
      (setq error-pos
	    (re-search-forward caml-interactive-toplevel-error-regexp
			       (point-max) t))
      (if error-pos
	  (setq beg (string-to-int (caml-match-string 1))
		end (string-to-int (caml-match-string 2)))))
    (if (not error-pos)
	(message "No syntax or typing error in last phrase.")
      (setq beg (+ caml-interactive-last-phrase-pos-in-toplevel beg)
	    end (+ caml-interactive-last-phrase-pos-in-toplevel end))
      (goto-char beg) (push-mark end t t))))

(defun caml-interrupt-caml ()
  (interactive)
  (if (comint-check-proc caml-interactive-buffer-name)
      (save-excursion
	(set-buffer caml-interactive-buffer-name)
	(comint-interrupt-subjob))))

(defun caml-kill-caml ()
  (interactive)
  (if (comint-check-proc caml-interactive-buffer-name)
      (save-excursion
	(set-buffer caml-interactive-buffer-name)
	(comint-kill-subjob))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Menu support

(defun caml-about () (interactive)
  (describe-variable 'caml-mode-version))
(defun caml-help () (interactive)
  (describe-function 'caml-mode))
(defun caml-interactive-help () (interactive)
  (describe-function 'caml-interactive-mode))

(defvar caml-definitions-menu-last-buffer nil)
(defvar caml-definitions-keymaps nil)

(defun caml-build-menu ()
  (if (condition-case nil (prog2 (require 'easymenu) nil) (error t)) ()
    (easy-menu-define
     caml-mode-menu (list caml-mode-map)
     "Caml Mode Menu."
     '("Caml"
       ("Interactive Mode"
	["Run Caml Toplevel" caml-run-caml t]
	["Interrupt Caml Toplevel" caml-interrupt-caml
	 :active (comint-check-proc caml-interactive-buffer-name)]
	["Kill Caml Toplevel" caml-kill-caml
	 :active (comint-check-proc caml-interactive-buffer-name)]
	["Evaluate Region" caml-eval-region
	 ;; region-active-p for XEmacs and mark-active for Emacs
	 :active (if (fboundp 'region-active-p) (region-active-p) mark-active)]
	["Evaluate Phrase" caml-eval-phrase t]
	["Evaluate Buffer" caml-eval-buffer t])
       ("Caml Forms"
	 ["try .. with .." caml-insert-try-form t]
	 ["match .. with .." caml-insert-match-form t]
	 ["let .. in .." caml-insert-let-form t]
	 ["if .. then .. else .." caml-insert-if-form t]
	 ["while .. do .. done" caml-insert-while-form t]
	 ["for .. do .. done" caml-insert-for-form t]
	 ["begin .. end" caml-insert-begin-form t])
       ["Switch .ml/.mli" caml-find-alternate-file t]
       "---"
       ["Compile..." compile t]
       ["Reference Manual..." caml-browse-manual t]
       ["Caml Library..." caml-browse-library t]
       ("Definitions"
	["Scan..." caml-list-definitions t])
       "---"
       ["Customize Caml Mode..." (customize-group 'caml) t]
       ("Caml Options" ["Dummy" nil t])
       ("Caml Interactive Options" ["Dummy" nil t])
       "---"
       ["About" caml-about t]
       ["Help" caml-help t]))
    (easy-menu-add caml-mode-menu)
    (caml-update-options-menu)
    ;; save and update definitions menu
    (make-local-variable 'caml-definitions-menu)
    (if (or caml-with-xemacs
	    (not (functionp 'easy-menu-create-keymaps))) ()
      ;; patch for Emacs
      (add-hook 'menu-bar-update-hook
		'caml-with-emacs-update-definitions-menu)
      (make-local-variable 'caml-definitions-keymaps)
      (setq caml-definitions-keymaps
	    (cdr (easy-menu-create-keymaps
		  "Definitions" caml-definitions-menu)))
      (setq caml-definitions-menu-last-buffer nil))))

(defun caml-interactive-build-menu ()
  (if (condition-case nil (prog2 (require 'easymenu) nil) (error t))
      ()
    (easy-menu-define
     caml-interactive-mode-menu (list caml-interactive-mode-map)
     "Caml Interactive Mode Menu."
     '("Caml"
       ("Interactive Mode"
	["Run Caml Toplevel" caml-run-caml t]
	["Interrupt Caml Toplevel" caml-interrupt-caml
	 :active (comint-check-proc caml-interactive-buffer-name)]
	["Kill Caml Toplevel" caml-kill-caml
	 :active (comint-check-proc caml-interactive-buffer-name)]
	["Evaluate Region" caml-eval-region :active (region-active-p)]
	["Evaluate Phrase" caml-eval-phrase t]
	["Evaluate Buffer" caml-eval-buffer t])
       "---"
       ["Customize Caml Mode..." (customize-group 'caml) t]
       ("Caml Options" ["Dummy" nil t])
       ("Caml Interactive Options" ["Dummy" nil t])
       "---"
       ["About" caml-about t]
       ["Help" caml-interactive-help t]))
    (easy-menu-add caml-interactive-mode-menu)
    (caml-update-options-menu)))

(defun caml-update-definitions-menu ()
  (easy-menu-change
   '("Caml") "Definitions"
   caml-definitions-menu))

;;   caml-definitions-menu)

(defun caml-with-emacs-update-definitions-menu ()
  (if (current-local-map)
      (let ((keymap
	     (lookup-key (current-local-map) [menu-bar Caml Definitions])))
	(if (and
	     (keymapp keymap)
	     (not (eq caml-definitions-menu-last-buffer (current-buffer))))
	    (setcdr keymap caml-definitions-keymaps)
	  (setq caml-definitions-menu-last-buffer (current-buffer))))))

(defun caml-toggle-option (symbol)
  (interactive)
  (set symbol (not (symbol-value symbol)))
  (if (eq 'caml-use-abbrev-mode symbol)
      (abbrev-mode caml-use-abbrev-mode)) ; toggle abbrev minor mode
  (if caml-with-xemacs nil (caml-update-options-menu)))

(defun caml-update-options-menu ()
  (easy-menu-change
   '("Caml") "Caml Options"
   (mapcar (lambda (pair)
	     (if (consp pair)
		 (vector (car pair)
			 (list 'caml-toggle-option (cdr pair))
			 ':style 'toggle
			 ':selected (nth 1 (cdr pair))
			 ':active t)
	       pair)) caml-options-list))
  (easy-menu-change
   '("Caml") "Caml Interactive Options"
   (mapcar (lambda (pair)
	     (if (consp pair)
		 (vector (car pair)
			 (list 'caml-toggle-option (cdr pair))
			 ':style 'toggle
			 ':selected (nth 1 (cdr pair))
			 ':active t)
	       pair)) caml-interactive-options-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Manual

;; From M. Quercia

(defun caml-browse-manual ()
  "*Browse Caml reference manual."
  (interactive)
  (setq caml-manual-url (read-from-minibuffer "URL: " caml-manual-url))
  (funcall caml-browser caml-manual-url))

(defun caml-xemacs-w3-manual (url)
  "*Browse Caml reference manual."
  (w3-fetch-other-frame url))

(defun caml-netscape-manual (url)
  "*Browse Caml reference manual."
  (start-process-shell-command
   "netscape" nil
   (concat "netscape -remote 'openURL ("
	   url ", newwindow)' || netscape " url)))

(defun caml-mmm-manual (url)
  "*Browse Caml reference manual."
  (start-process-shell-command
   "mmm" nil
   (concat "mmm_remote " url " || mmm -external " url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Library

;; From M. Quercia

(defun caml-browse-library()
  "Browse the Caml library."
  (interactive)
  (let ((buf-name "*caml-library*") (opoint)
	(dir (read-from-minibuffer "Library path: " caml-library-path)))
    (if (and (file-directory-p dir) (file-readable-p dir))
	(progn
	  (setq caml-library-path dir)
	  ;; list *.ml and *.mli files
	  (with-output-to-temp-buffer buf-name
	    (buffer-disable-undo standard-output)
	    (save-excursion
	      (set-buffer buf-name)
	      (kill-all-local-variables)
	      (make-local-variable 'caml-library-path)
	      (setq caml-library-path dir)
	      ;; help
	      (insert "Directory \"" dir "\".\n") 
	      (insert "Select a file with middle mouse button or RETURN.\n\n")
	      (insert "Interface files (.mli):\n\n")
	      (insert-directory (concat dir "/*.mli") "-C" t nil)
	      (insert "\n\nImplementation files (.ml):\n\n")
	      (insert-directory (concat dir "/*.ml") "-C" t nil)
	      ;; '.', '-' and '_' are now letters
	      (modify-syntax-entry ?. "w")
	      (modify-syntax-entry ?- "w")
	      (modify-syntax-entry ?_ "w")
	      ;; every file name is now mouse-sensitive
	      (goto-char (point-min))
	      (while (< (point) (point-max))
		(re-search-forward "\\.ml.?\\>")
		(setq opoint (point))
		(re-search-backward "\\<" (point-min) 1)
		(put-text-property (point) opoint 'mouse-face 'highlight)
		(goto-char (+ 1 opoint)))
	      ;; activate caml-library mode
	      (setq major-mode 'caml-library-mode)
	      (setq mode-name "caml-library")
	      (use-local-map caml-library-mode-map)
	      (setq buffer-read-only t)))))))
  
(setq caml-library-mode-map (make-keymap))
(suppress-keymap caml-library-mode-map)
(define-key caml-library-mode-map [return] 'caml-library-find-file)
(define-key caml-library-mode-map [mouse-2] 'caml-library-mouse-find-file)
  
(defun caml-library-find-file ()
  "Load the file whose name is near point."
  (interactive)
  (save-excursion
    (if (text-properties-at (point))
	(progn
	  (re-search-backward "\\<") (setq beg (point))
	  (re-search-forward "\\>")
	  (find-file-read-only (concat caml-library-path "/"
				       (buffer-substring-no-properties
					beg (point))))))))

(defun caml-library-mouse-find-file (event)
  "Visit the file name you click on."
  (interactive "e")
  (let ((owindow (selected-window)))
    (mouse-set-point event)
    (caml-library-find-file)
    (select-window owindow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Definitions List

;; Designed from original code by M. Quercia

(defconst caml-definitions-regexp
  "\\<\\(and\\|val\\|value\\|type\\|module\\|class\\|exception\\|let\\)\\>"
  "Regexp matching definition phrases.")

(defconst caml-definitions-bind-skip-regexp
  "\\<\\(rec\\|type\\|virtual\\)\\>\\|'[A-Za-z\300-\377][0-9_'A-Za-z\300-\377]*\\|('.*)"
  "Regexp matching stuff to ignore after a binding keyword.")

(defvar caml-definitions-menu (list ["Scan..." caml-list-definitions t])
  "Initial content of the definitions menu.")

(defun caml-list-definitions ()
  "Parses the buffer and gathers toplevel definitions for quick
jump via the definitions menu'."
  (interactive)
  (message "Searching definitions...")
  (save-excursion
    (let ((cpt 0) (kw) (menu)
	  (value-list) (type-list) (module-list) (class-list) (misc-list))
      (goto-char (point-min))
      (caml-skip-blank-and-comments)
      (while (< (point) (point-max))
	(if (looking-at caml-definitions-regexp)
	    (progn
	      (setq kw (caml-match-string 0))
	      (if (string= kw "and")
		  (setq kw (save-match-data
			     (save-excursion (caml-find-and-match)))))
	      (if (or (string= kw "exception")
		      (string= kw "val")
		      (string= kw "value")) (setq kw "let"))
	      ;; skip optional elements
	      (goto-char (match-end 0))
	      (caml-skip-blank-and-comments)
	      (if (looking-at caml-definitions-bind-skip-regexp)
		  (goto-char (match-end 0)))
	      (caml-skip-blank-and-comments)
	      (if (looking-at "\\<[A-Za-z\300-\377][0-9_'A-Za-z\300-\377]*\\>")
		  ;; menu item : [name (goto-char ...) t]
		  (let* ((p (make-marker))
			 (ref (vector (caml-match-string 0)
				      (list 'caml-goto p) t)))
		    (setq cpt (1+ cpt))
		    (message (concat "Searching definitions... ("
				     (number-to-string cpt) ")"))
		    (set-marker p (point))
		    (cond
		     ((string= kw "let")
		      (setq value-list (cons ref value-list)))
		     ((string= kw "type")
		      (setq type-list (cons ref type-list)))
		     ((string= kw "module")
		      (setq module-list (cons ref module-list)))
		     ((string= kw "class")
		      (setq class-list (cons ref class-list)))
		     (t (setq misc-list (cons ref misc-list))))))))
	;; skip to next phrase or next top-level `and'
	(caml-forward-char)
	(caml-next-phrase t)
	(let ((old-point (point)) (last-and))
	  (setq last-and (point))
	  (save-excursion
	    (while (and (re-search-backward "\\<and\\>" old-point t)
			(not (caml-in-literal-or-comment-p))
			(save-excursion (caml-find-and-match)
					(>= old-point (point))))
	      (setq last-and (point))))
	  (goto-char last-and)))
      ;; sort and build lists
      (mapcar (lambda (pair)
		(if (cdr pair)
		    (setq menu
			  (append (caml-split-long-list
			    (car pair) (caml-sort-definitions (cdr pair)))
				  menu))))
	      (list (cons "Miscellaneous" misc-list)
		    (cons "Values" value-list)
		    (cons "Classes" class-list)
		    (cons "Types" type-list)
		    (cons "Modules" module-list)))
      ;; update definitions menu
      (setq caml-definitions-menu
	    (append menu (list "---" ["Rescan..." caml-list-definitions t])))
      (if (or caml-with-xemacs
	      (not (functionp 'easy-menu-create-keymaps))) ()
	;; patch for Emacs 20.2
	(setq caml-definitions-keymaps
	      (cdr (easy-menu-create-keymaps 
		    "Definitions" caml-definitions-menu)))
	(setq caml-definitions-menu-last-buffer nil))
      (message "Searching definitions... done")))
  (caml-update-definitions-menu))

(defun caml-goto (pos)
  (goto-char pos)
  (recenter))

(defun caml-sort-definitions (list)
  (let* ((last "") (cpt 1)
	 (list (sort (nreverse list)
		     (lambda (p q) (string< (elt p 0) (elt q 0)))))
	 (tail list))
    (while tail
      (if (string= (elt (car tail) 0) last)
	  (prog2
	      (setq cpt (1+ cpt))
	      (aset (car tail) 0 (format "%s (%d)" last cpt)))
	(setq cpt 1)
	(setq last (elt (car tail) 0)))
      (setq tail (cdr tail)))
    list))

;; look for the (n-1)th or last element of a list
(defun caml-nth (n list)
  (if (or (<= n 1) (null list) (null (cdr list))) list
    (caml-nth (1- n) (cdr list))))
    
;; split a definition list if it is too long
(defun caml-split-long-list (title list)
  (let ((tail (caml-nth caml-definitions-max-items list)))
    (if (or (null tail) (null (cdr tail)))
        ;; list not too long, cons the title
        (list (cons title list))
      ;; list too long, split and add initials to the title
      (let (lists)
        (while list
          (let ((beg (substring (elt (car list) 0) 0 1))
                (end (substring (elt (car tail) 0) 0 1)))
            (setq lists (cons
                         (cons (format "%s %s-%s" title beg end) list)
                         lists))
            (setq list (cdr tail))
            (setcdr tail nil)
            (setq tail (caml-nth caml-definitions-max-items list))))
        (nreverse lists)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Hooks and Exit

(defvar caml-load-hook nil
  "This hook is run when Caml is loaded in. It is a good place to put
key-bindings or hack Font-Lock keywords...")

(run-hooks 'caml-load-hook)

(provide 'caml) ;; for compatibility with caml support modes
                ;; you may also link caml.el to caml.el
(provide 'caml)
