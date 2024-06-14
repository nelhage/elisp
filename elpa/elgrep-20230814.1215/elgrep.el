;;; elgrep.el --- Searching files for regular expressions -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Tobias Zawada

;; Author: Tobias Zawada <i@tn-home.de>
;; Keywords: tools, matching, files, unix
;; Version: 1.0.0
;; URL: https://github.com/TobiasZawada/elgrep
;; Package-Requires: ((emacs "26.2") (async "1.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Open the `elgrep-menu' via menu item "Tools" -> "Search files (Elgrep)...".
;; There are menu items for the directory, the file name regexp for filtering
;; and the regexp for grepping.
;; Furthermore, you can also switch on recursive grep.
;;
;; Run M-x elgrep to search a single directory for files with file
;; name matching a given regular expression for text matching a given
;; regular expression.
;; With prefix arg it searches the directory recursively.

;;; Changes:

;; 2019-08-17:
;; - Introduce new keymap `elgrep-menu-button-map' where <mouse-2>
;;   is defined as `widget-button-click'.
;; - Replace macro `elgrep-menu-with-buttons' with
;;   new button type `elgrep-push-button'.
;; - New function `elgrep-widget-replace'.
;;   This function can replace the start button
;;   with a stop button and change its action
;;   to kill the asynchronous emacs process.
;; 2019-09-09:
;; - Add `elgrep-menu-call-list' and related functions.
;; 2019-10-13:
;; - allow lists of regular expressions for the re argument of `elgrep'
;; - add the concept of records
;;   A file can be split into records by regexps or functions.
;;   Each of the records is searched for all regexps in the re gargument
;;   of `elgrep'.
;; - Correct handling of `elgrep-data-file' such that asynchronous
;;   calls of recursive elgrep work again.
;; 2019-10-20:
;; - Allow lisp forms as c-beg, c-end, r-beg, and r-end.
;; 2019-11-07:
;; - Optionally follow symlinks (elgrep option :symlink)
;; - Enable saving of the elgrep-menu buffer
;; - Bugfix: Correct order of results of `elgrep-occur-search'

;;; Code:

(require 'widget)
(eval-when-compile
  (require 'subr-x))

(require 'wid-edit) ; for widget-at and widget-value-set

(declare-function dired-build-subdir-alist "dired")
(declare-function elgrep-menu-hist-move "elgrep" (dir) t)

(require 'cl-lib)
(require 'easymenu)
(require 'grep) ; also provides "compile"
(require 'async)

(defcustom elgrep-data-file "elgrep-data.el"
  "Path for storing elgrep data in Elisp format.
A form setting `elgrep-call-list' is stored there when Emacs is killed.
That file is run when \"elgrep.el\" is loaded.

If the path is not absolute it is relative to `user-emacs-directory'.

If `elgrep-data-file' is nil nothing is saved and loaded."
  :group 'elgrep
  :type '(choice (const nil) file))

(defcustom elgrep-negating-char ?!
  "Character indicating negation in the required matches."
  :type 'characterp
  :group 'elgrep)

(defcustom elgrep-log-buffer "*elgrep-log*"
  "Buffer where `elgrep-log' writes its output.
Can be a buffer or a buffer name.
The default value is the default buffer name as string."
  :group 'elgrep
  :type 'string)

(defvar elgrep-call-list nil
  "List of calls to `elgrep-menu-elgrep'.
Each call is a cons of a name string (maybe empty for unnamed)
and an evaluable elgrep form.")

;; For safity reasons `elgrep-edit' exploits the text properties
;; `elgrep-context', `elgrep-context-begin', `elgrep-context-end'
;; This implies that `elgrep-edit' does not work anymore with
;; `grep-mode' allone.
;; Therefore, we introduce a new mode for listing the elgrep matches:
;;;###autoload
(define-derived-mode elgrep-mode grep-mode "elgrep"
  "Major mode for elgrep buffers.
See `elgrep' and `elgrep-menu' for details."
  (setq header-line-format (substitute-command-keys "Quit (burry-buffer): \\[quit-window]; go to occurence: \\[compile-goto-error]; elgrep-edit-mode: \\[elgrep-edit-mode]")))

(define-key elgrep-mode-map (kbd "C-c C-e") #'elgrep-edit-mode)
(define-key elgrep-mode-map (kbd "g") #'elgrep-rerun)

(defun elgrep-raw-contents-of-string (string)
  "Copy raw contents of STRING.
Substrings with 'elgrep property are removed."
  (let ((target "")
	b
	(pt 0)
	(e (length string)))
    (while (< pt e)
      (setq b (next-single-char-property-change pt 'elgrep string e))
      (unless (get-char-property pt 'elgrep string)
	(setq target (concat target (substring string pt b))))
      (setq pt b))
    target))
;; Test:
;; (elgrep-raw-contents-of-string "first test")
;; (elgrep-raw-contents-of-string (concat (propertize "first" 'elgrep 'context) " test"))
;; (elgrep-raw-contents-of-string "")

(defun elgrep-raw-contents (&optional b e)
  "Copy raw contents of elgrep buffer from B to E.
B and E default to the beginning and the end of the buffer
if region is not active otherwise to the beginning and the end
of the active region.

B can also be a string.
In that case return that part of string B without
any 'elgrep text property.
Function `elgrep-raw-contents-of-string' is used with B as argument."
  (interactive)
  (if (stringp b)
      (elgrep-raw-contents-of-string b)
    (unless b (setq b (if (use-region-p)
			  (region-beginning)
			(point-min))))
    (unless e (setq e (if (use-region-p)
			  (region-end)
			(point-max))))
    (let ((target (get-buffer-create "*elgrep-raw*")))
      (with-current-buffer target
	(erase-buffer))
      (save-excursion
	(goto-char b)
	(while (< (point) e)
	  (setq b (next-single-char-property-change (point) 'elgrep nil e))
	  (unless (get-char-property (point) 'elgrep)
	    (princ (buffer-substring (point) b) target))
	  (goto-char b))
	(kill-new (with-current-buffer target (buffer-string)))))))

(easy-menu-define nil elgrep-mode-map
  "Menu for `elgrep-mode'."
  '("Elgrep"
    ["Next Match" compilation-next-error :enable t :help "Visit the next match and corresponding location"]
    ["Previous Match" compilation-previous-error :enable t :help "Visit the previous match and corresponding location"]
    ["Kill Match" elgrep-kill-compilation-message :enable t :help "Kill match at point"]
    ["First Match" first-error :enable t :help "Restart at the first match, visit corresponding location"]
    ["Display Match at Point" next-error-follow-minor-mode :enable t :style toggle :selected next-error-follow-minor-mode :help "Update the display of the match when you move point"]
    "--"
    ["Elgrep-edit" elgrep-edit-mode :enable t :style toggle :selected elgrep-edit-mode :help "Toggle Elgrep Edit Mode for editing matches and saving them"]
    ["Copy raw contents" elgrep-raw-contents :enable t :help "Get contents of active region of the elgrep buffer without header line, file names and line numbers"]
    ["Rerun Elgrep" elgrep-rerun :enable t :help "Rerun Elgrep, C-u: Read command from minibuffer with previous command as default input"]))

(define-key elgrep-mode-map (kbd "<menu-bar> <grep>") nil)
(define-key elgrep-mode-map (kbd "<menu-bar> <compilation>") nil)

(defun elgrep-at-property-boundary-p (position property &optional object)
  "Non-nil if POSITION is at an interval boundary of text PROPERTY.
That means that POSITION is 1 or the PROPERTY value before POSITION
is different to that at POSITION.
If OBJECT can be a string or a buffer or nil, standing for the
current buffer."
  (or (eq position (if (stringp object) 0 1))
      (null (eq (get-text-property position property object)
		(get-text-property (1- position) property object)))))

(defun elgrep-mode-at-context-p (&optional pos)
  "Return non-nil if POS is inside a context region of a match."
  (unless pos (setq pos (point)))
  (get-char-property pos 'compilation-message))

(defun elgrep-mode-beginning-of-match ()
  "Go to the beginning of current match."
  (interactive)
  (unless (get-text-property (point) 'compilation-message)
    (dotimes (_i 2)
      (goto-char (previous-single-char-property-change (point) 'compilation-message))))
  (when (get-text-property (point) 'compilation-message)
    (unless (elgrep-at-property-boundary-p (point) 'compilation-message)
      (goto-char (previous-single-char-property-change (point) 'compilation-message)))
    t))

(defun elgrep-mode-match-beginning-position ()
  "Position of the beginning of current match."
  (save-excursion
    (when (elgrep-mode-beginning-of-match)
      (point))))

(defun elgrep-mode-match-end-position ()
  "Position of the beginning of current match."
  (save-excursion
    (when (elgrep-mode-next-match)
      (backward-char))
    (point)))

(defun elgrep-mode-next-match (&optional n limit)
  "Go to next match within LIMIT and return non-nil if there is one.
LIMIT defaults to `point-max'.
With positive whole number N go forward N times."
  (interactive "p")
  (unless n (setq n 1))
  (when (< n 0)
    (user-error "Number of repetations must be non-negative"))
  (unless limit (setq limit (point-max)))
  (if (eq n 1)
      (progn
	(when (get-text-property (point) 'compilation-message)
	  (goto-char (next-single-property-change (point) 'compilation-message nil limit)))
	(goto-char (next-single-property-change (point) 'compilation-message nil limit)))
    (while (null (zerop n))
      (cl-decf n)
      (elgrep-mode-next-match 1 limit)))
  (get-text-property (point) 'compilation-message))

(defun elgrep-mode-in-match-p (&optional pos)
  "Return non-nil if POS is in match.
The value returned is the `compilation-message' for the match."
  (unless pos (setq pos (point)))
  (or (elgrep-mode-at-context-p pos)
      (get-text-property
       (max 1 (1- (previous-single-char-property-change pos 'compilation-message)))
       'compilation-message)))

(defun elgrep-mode-get-match (&optional pos)
  "Get file match at POS.
POS defaults to `point'.
File matches are described
in the doc string of `elgrep-search'."
  (save-excursion
    (when pos
      (goto-char pos))
    (when-let (((elgrep-mode-beginning-of-match))
	       (msg (get-text-property (point) 'compilation-message))
	       (loc (compilation--message->loc msg))
	       (struct (compilation--loc->file-struct loc))
	       (spec (compilation--file-struct->file-spec struct))
	       (name (car spec))
	       (line (compilation--loc->line loc))
	       (match-string (elgrep-raw-contents-of-string (buffer-substring (point) (elgrep-mode-match-end-position))))
	       (ctx (get-text-property (point) 'elgrep-context))
	       (ctx-beg (get-text-property (point) 'elgrep-context-begin))
	       (ctx-end (get-text-property (point) 'elgrep-context-end)))
      (list
       name
       (list ;; list of sub-matches
	(list
	 :match ""
	 :line line
	 :context ctx
	 :beg ctx-beg
	 :end ctx-beg
	 :context-beg ctx-beg
	 :context-end ctx-end
	 ))))))

(defun elgrep-mode-matches (&optional b e)
  "Return list with every file match contained in region B E."
  (unless b (setq b (if (use-region-p) (region-beginning) (point-min))))
  (unless e (setq e (if (use-region-p) (region-end) (point-max))))
  (let (matches)
    (save-excursion
      (goto-char b)
      (when (if (elgrep-mode-at-context-p)
		(elgrep-mode-beginning-of-match)
	      (elgrep-mode-next-match))
	(setq matches (list (elgrep-mode-get-match)))
	(while (elgrep-mode-next-match 1 e)
	  (push (elgrep-mode-get-match) matches)
	  )))
    (nreverse matches)))

(defun elgrep-match/context (file-match)
  "Get the first context in FILE-MATCH.
See `elgrep-search' for the structure of FILE-MATCH."
  (let* ((matches (cdr file-match))
	 (first-match (car matches))
	 (first-submatch (car first-match))
	 (context (plist-get first-submatch :context)))
    (unless (stringp context)
      (user-error "First context in match not found"))
    context))

(defun elgrep-matches/sort-by-context (match-list)
  "Sort file MATCH-LIST w.r.t. the the first context."
  (sort match-list
	(lambda (m1 m2)
	  (string<
	   (elgrep-match/context m1)
	   (elgrep-match/context m2))
          )))

(defun elgrep-symbol-completion-in-minibuffer ()
  "Complete symbol in minibuffer."
  (let ((b (line-beginning-position))
	(e (point)))
    (when (string-match lisp-mode-symbol-regexp (buffer-substring-no-properties b e))
      (list b e obarray :predicate #'functionp))))

(defun elgrep-read-expression (prompt &optional initial-contents history)
  "Print PROMPT and read expression from minibuffer.
Thereby, use INITIAL-CONTENTS for `read-from-minibuffer', which see.
Default for HISTORY is `read-expression-history'."
  ;; This is mainly a copy of `read--expression'.
  ;; But, we do not use `read--expression' here because
  ;; that function is marked as internal and may change any time.
  (unless history (setq history 'read-expression-history))
  (let ((minibuffer-completing-symbol t))
    (minibuffer-with-setup-hook
        (lambda ()
          ;; FIXME: call emacs-lisp-mode (see also
          ;; `eldoc--eval-expression-setup')?
          (add-hook 'completion-at-point-functions
                    #'elisp-completion-at-point nil t)
          (add-hook 'completion-at-point-functions
                    #'elgrep-symbol-completion-in-minibuffer nil t)
          (run-hooks 'eval-expression-minibuffer-setup-hook))
      (read-from-minibuffer prompt initial-contents
                            read-expression-map t
                            history))))

(defmacro elgrep-with-partial-history (history predicate &rest body)
  "Evaluate BODY with history in HISTORY reduced to items fulfilling PREDICATE."
  (declare (debug (symbolp form &rest form)) (indent 2))
  (let ((hist-first (make-symbol "hist-first"))
	(ret (make-symbol "ret"))
	(oldhist (make-symbol "oldhist")))
    `(if history
	 (let (,ret
	       ,hist-first
	       (,oldhist (symbol-value ,history)))
	   (unwind-protect
	       (progn
		 (set
		  ,history (cl-remove-if-not ,predicate (cl-copy-list (symbol-value ,history))))
		 (setq ,hist-first (list (car (symbol-value ,history)))
		       ,ret (progn ,@body))
		 (if (eq (car ,hist-first) (car (symbol-value ,history)))
		     (setq ,hist-first nil)
		   (setq ,hist-first (list (car (symbol-value ,history))))))
	     (set ,history ,oldhist))
	   (when ,hist-first
	     (let ((history-delete-duplicates t))
	       (add-to-history ,history (car ,hist-first))))
	   ,ret)
       ,@body)))

(defun elgrep-read-regexp-or-function (prompt &optional defaults history)
  "Read a regexp or function from minibuffer.
PROMPT should be a string and
HISTORY should be a symbol of a history variable.
DEFAULTS is the corresponding arg of `read-regexp'."
  (cl-case (car
	    (read-multiple-choice prompt
				  '((?r "regular expression") (?f "function"))))
    (?r
     (elgrep-with-partial-history
	 history
	 #'stringp
       (read-regexp (format "%s regexp: " prompt) defaults history)))
    (?f
     (if history
	 (elgrep-with-partial-history
	     history
	     #'functionp
	   (set history (mapcar (lambda (item)
				  (format "%S" item))
				(symbol-value history)))
	   (elgrep-read-expression (format "%s function: " prompt) nil history)
	   (setcar (symbol-value history) (car (read-from-string (car (symbol-value history))))))
       (elgrep-read-expression (format "%s function: " prompt))))
    ))

(defvar-local elgrep-args nil
  "Arguments `elgrep' is called with.
Used in the '*elgrep*' buffer.")

(defun elgrep-mode-transform (fun)
  "Transform match list of elgrep-buffer by FUN.
FUN is a function that gets the list of MATCHES
of the current buffer and should return the transformed list.
See function `elgrep-search' for the structure of MATCHES."
  (interactive (list (elgrep-read-expression "Transformation function: ")))
  (let* ((matches (elgrep-mode-matches)))
    (setq matches (funcall fun matches))
    (apply #'elgrep-show matches elgrep-args)))


(defvar elgrep-re-hist nil
  "History for elgrep regular expressions for `elgrep' (which see).
May also contain lists of regular expressions.")
(defvar elgrep-file-name-re-hist nil
  "History of file-name regular expressions for `elgrep' (which see).")
(defvar elgrep-exclude-file-re-history nil
  "History of exclusion regexp for file names.")
(defvar elgrep-record-beg-re-hist nil
  "History for record begin regexps.")
(defvar elgrep-record-end-re-hist nil
  "History for record end regexps.")
(defvar elgrep-context-beg-re-hist nil
  "History for context begin regexps.")
(defvar elgrep-context-end-re-hist nil
  "History for context end regexps.")

(defun elgrep-log (format &rest args)
  "Log string formatted with FORMAT and ARGS in `elgrep-log-buffer'."
  (with-current-buffer
      (get-buffer-create (or elgrep-log-buffer "*elgrep-log*"))
    (insert (format format args))))

(defun elgrep-log-file-error (err msg &rest args)
  "Report file-error ERR with MSG and ARGS in `elgrep-log-buffer'."
  (apply #'elgrep-log msg args)
  (let ((msg (get (car err) 'error-message)))
    (if msg
	(elgrep-log ":\n%s\n" msg)
      (elgrep-log ".\n"))))

(defun elgrep-insert-file-contents (filename &optional visit)
  "Like `insert-file-contents' for FILENAME.
It uses `pdftotext' (poppler) for pdf-files (with file extension pdf).
VISIT is passed as second argument to `insert-file-contents'."
  (condition-case err
      (if (string-match  "\.pdf\\'" (downcase (file-name-extension filename t)))
	  (call-process "pdftotext" filename (current-buffer) visit "-" "-")
	(insert-file-contents filename visit))
    (file-error
     (elgrep-log-file-error err "Error while reading file %S" filename)
     nil)))

(defun elgrep-dired-files (files)
  "Print FILES in `dired-mode'."
  (insert "  " default-directory ":\n  elgrep 0\n")
  (dolist (file files)
    (let ((a (file-attributes file 'string)))
      (insert (format "  %s %d %s %s %6d %s %s\n"
		      (nth 8 a) ; file modes like ls -l
		      (nth 1 a) ; number of links to file
		      (nth 2 a) ; uid as string
		      (nth 3 a) ; gid as string
		      (nth 7 a) ; size in bytes
		      (format-time-string "%d. %b %Y" (nth 5 a)) ; modification time
		      file))))
  (dired-mode)
  (dired-build-subdir-alist))

(defmacro elgrep-line-position (limiter match-bound ctx-bound c-only pos-op search-op)
  "If LIMITER is a number act like (POS-OP (1+ LIMITER)).
Thereby count lines starting at MATCH-BOUND.
POS-OP is either `line-end-position' or `line-beginning-position'.
If LIMITER is a regular expression search with SEARCH-OP for that RE
starting at CTX-BOUND
and return `line-end-position' or `line-beginning-position'
of the line with the match, respectively.
If LIMITER is a function call it with no args.
LIMITER is supposed to move point to the boundary of the context
or return nil if that boundary cannot be found.
If C-ONLY is nil the context is extended to the next linebreak
in the appropriate direction which is determined by POS-OP."
  `(save-excursion
     (cond
      ((stringp ,limiter)
       (goto-char ,ctx-bound)
       (save-match-data
	 (when (,search-op ,limiter nil t) ;; t=noerror
	   (if ,c-only
	       (point)
	     (,pos-op)))
	 ))
      ((numberp ,limiter)
       (goto-char ,match-bound)
       (if ,c-only
	   (progn
	     (forward-char ,limiter)
	     (point))
	 (,pos-op (1+ ,limiter))))
      ((functionp ,limiter)
       (goto-char ,ctx-bound)
       (save-match-data
	 (when (funcall ,limiter)
	   (if ,c-only
	       (point)
	     (,pos-op)))))
      (t
       (goto-char ,ctx-bound)
       (save-match-data
	 (when (eval ,limiter)
	   (if ,c-only
	       (point)
	     (,pos-op))
	   ))))))

(defun elgrep-classify (classifier list &rest options)
  "Use CLASSIFIER to map the LIST entries to class denotators.
Returns the list of equivalence classes.  Each equivalence class
is a cons whose `car' is the class denotator and the cdr is the
list of members.

Accept a plist of OPTIONS.
Keywords supported: :test"
  (let ((test (or (plist-get options :test) 'equal)))
    (let (classify-res)
      (dolist (classify-li list)
	(let* ((classify-key (funcall classifier classify-li))
	       (classify-class (cl-assoc classify-key classify-res :test test)))
	  (if classify-class
	      (setcdr classify-class (cons classify-li (cdr classify-class)))
	    (setq classify-res (cons (list classify-key classify-li) classify-res)))))
      classify-res)))

(defun elgrep-default-filename-regexp (&optional dir)
  "Create default filename regexp from DIR.
Use the most often in DIR used extension for the regexp.
DIR defaults to `default-directory'."
  (unless dir (setq dir default-directory))
  (let* ((filelist (cl-delete-if (lambda (file) (string-match "\\.\\(~\\|bak\\)\\'" file))
				 (directory-files dir)))
	 (ext (car-safe (cl-reduce (lambda (x y) (if (> (length x) (length y)) x y)) (elgrep-classify 'file-name-extension filelist))));; most often used extension
	 )
    (concat "\\." ext "\\'")))

(defvar elgrep-w-dir)
(defvar elgrep-w-file-name-re)
(defvar elgrep-w-re)
(defvar elgrep-w-recursive)
(defvar elgrep-w-symlink)
(defvar elgrep-w-mindepth)
(defvar elgrep-w-maxdepth)
(defvar elgrep-w-r-beg)
(defvar elgrep-w-r-end)
(defvar elgrep-w-c-beg)
(defvar elgrep-w-c-beg-only)
(defvar elgrep-w-c-end)
(defvar elgrep-w-c-end-only)
(defvar elgrep-w-case-fold-search)
(defvar elgrep-w-exclude-file-re)
(defvar elgrep-w-dir-re)
(defvar elgrep-w-exclude-dir-re)
(defvar elgrep-w-buffer-init)
(defvar elgrep-w-file-fun)
(defvar elgrep-w-search-fun)
(defvar elgrep-w-async)
(defvar elgrep-w-call-list)
(defvar-local elgrep-w-start nil
  "Start button widget of `elgrep-menu' buffer.")
(defvar-local elgrep-menu-id 0
  "Unique id for `elgrep-menu' buffer.")

(defvar-local elgrep-widget-list nil
  "List of widgets generated by `elgrep-widget-create'.")

(defun elgrep-widget-create (&rest args)
  "Like `widget-create' with ARGS but add :initial-value property.
The :initial-value property is the :value
right after the call of `widget-create'."
  (let ((wid (apply #'widget-create args)))
    (widget-put wid :initial-value (widget-value wid))
    (cl-pushnew wid elgrep-widget-list)
    wid))

(defun elgrep-menu-reset ()
  "Set widgets in `widget-field-list' to their :initial-value if that is set."
  (dolist (wid elgrep-widget-list)
    (when (widget-member wid :initial-value)
      (widget-value-set wid (widget-get wid :initial-value)))))

(defun elgrep-widget-value-modified-p (wid)
  "Check whether :value of WID differs from its :initial-value.
Comparison done with `equal'."
  (null (equal (widget-value wid) (widget-get wid :initial-value))))

(defun elgrep-widget-value-update-hist (wid)
  "Get value of widget WID and update its :prompt-history variable."
  (when-let ((ret (widget-value wid))
	     (hist-var (widget-get wid :prompt-history))
	     (hist-length (or (get hist-var 'history-length) history-length)))
    (unless (equal ret (car-safe (symbol-value hist-var)))
      (set hist-var (cons ret (symbol-value hist-var)))
      (when (> (length (symbol-value hist-var)) hist-length)
	(setf (nthcdr hist-length (symbol-value hist-var)) nil)))
    ret))

(defun elgrep-widget-set-choice (widget current &optional value)
  "Set `menu-choice' WIDGET to CURRENT with VALUE.
CURRENT should be one of the elements of (widget-get widget :args)."
  (widget-put widget :explicit-choice current)
  (widget-value-set widget value)
  (widget-setup)
  (widget-apply widget :notify widget nil))

(defun elgrep-menu-record-p (rec)
  "Check whether REC is an admissible value for `elgrep-w-r-end'."
  (or
   (eq rec t)
   (stringp rec)
   (functionp rec)
   (listp rec) ;; TODO: `listp' is rather unspecific.
   ))

(defun elgrep-menu-context-p (ctxt)
  "Check whether CTXT is an admissible value for `elgrep-w-c-beg'."
  (or (integerp ctxt)
      (stringp ctxt)
      (functionp ctxt)
      (listp ctxt)))

(defun elgrep-menu-async-p (async)
  "Check whether ASYNC is admissible for `elgrep-w-async'."
  (or (booleanp async)
      (eq async 'thread)))

(defsubst elgrep-string-or-function-p (val)
  "Return non-nil if VAL is a string or a function."
  (or (stringp val) (functionp val)))



(defconst elgrep-menu-arg-alist '((recursive booleanp (y-or-n-p "Recursive? "))
				  (symlink booleanp (y-or-n-p "Follow Symlinks? "))
				  (mindepth integerp (read-number "Minimal depth: "))
				  (maxdepth integerp (read-number "Maximal depth: "))
				  (r-beg elgrep-menu-record-p (elgrep-read-regexp-or-function "Record beginning" nil 'elgrep-record-beg-re-hist))
				  (r-end elgrep-menu-record-p (elgrep-read-regexp-or-function "Record end" nil 'elgrep-record-end-re-hist))
				  (c-beg elgrep-menu-context-p (elgrep-read-regexp-or-function "Context beginning" nil 'elgrep-context-beg-re-hist))
				  (c-beg-only booleanp (y-or-n-p "Use context beginning only, i.e., no extension to beginning of line? "))
				  (c-end elgrep-menu-context-p (elgrep-read-regexp-or-function "Context end" nil 'elgrep-context-end-re-hist))
				  (c-end-only booleanp (y-or-n-p "Use context end only, i.e., no extension to end of line? "))
				  (case-fold-search booleanp (y-or-n-p "Case-fold search? "))
				  (exclude-file-re elgrep-string-or-function-p (read-regexp "Exclude files matching regexp: "))
				  (dir-re stringp (read-regexp "Recurse dirs matching regexp: "))
				  (exclude-dir-re stringp (read-regexp "Exclude dirs matching regexp: "))
				  (async elgrep-menu-async-p (y-or-n-p "Run asynchronously? "))
				  (buffer-init (lambda (val)
						   (memq val '(nil syntax-table major-mode))))
				  (file-fun functionp)
				  (search-fun functionp (read (completing-read "Search function: " obarray #'functionp)))
				  (interactive ignore))
  "Alist mapping elgrep options to predicates and read functions.
Each OPTION is available in the `elgrep-menu' as elgrep-w-OPTION
and for the command `elgrep' as :OPTION.
There are some `elgrep' options that are not available as
menu option such as :interactive.
The option is ignored if the predicate is 'ignore.
There are also widgets that are not available as `elgrep' options
such as `elgrep-w-start'.")

;;;###autoload
(defun elgrep-menu-arg-list ()
  "Collect `elgrep' arguments from `elgrep-menu' buffer."
  (interactive "@")
  (append
   (list (elgrep-widget-value-update-hist elgrep-w-dir)
	 (elgrep-widget-value-update-hist elgrep-w-file-name-re)
	 (elgrep-widget-value-update-hist elgrep-w-re))
   (let (ret opt-name wid)
     (dolist (opt elgrep-menu-arg-alist)
       (setq opt-name (symbol-name (car opt)))
       (unless (eq (cadr opt) 'ignore)
	 (setq wid (symbol-value (intern-soft (concat "elgrep-w-" opt-name))))
	 (when (elgrep-widget-value-modified-p wid)
	   (setq ret
		 (cons (widget-value wid)
		       (cons (intern (concat ":" opt-name))
			     ret))))))
     (nreverse ret))))

(defvar-local elgrep-thread nil
  "Thread of the elgrep call with :async option 'thread.
Normally bound in the `elgrep-menu' buffer.")

(defun elgrep-menu-stop (&rest _ignore)
  "Stop elgrep process of current buffer.
If there is no elgrep process reset Start button."
  (interactive "@")
  (cond
   ((and (threadp elgrep-thread) (thread-live-p elgrep-thread))
    (thread-signal elgrep-thread 'quit nil))
   ((process-live-p (get-buffer-process (current-buffer)))
    (kill-process))) ;; The process sentinel resets the button.
   (elgrep-reset-start-button))

(defun elgrep-menu-elgrep (&rest _ignore)
  "Start `elgrep' with data from `elgrep-menu'."
  (interactive "@")
  (let* ((async (widget-value elgrep-w-async)))
    (when async
      (elgrep-widget-replace
       'elgrep-w-start
       'elgrep-push-button
       :value "Stop elgrep"
       :action #'elgrep-menu-stop
       ))
    (elgrep-menu-call-add-to-list elgrep-w-call-list (cons 'elgrep/i (elgrep-menu-arg-list)))
    (elgrep (elgrep-widget-value-update-hist elgrep-w-dir)
	    (elgrep-widget-value-update-hist elgrep-w-file-name-re)
	    (elgrep-widget-value-update-hist elgrep-w-re)
	    :recursive (widget-value elgrep-w-recursive)
	    :symlink (widget-value elgrep-w-symlink)
	    :mindepth (widget-value elgrep-w-mindepth)
	    :maxdepth (widget-value elgrep-w-maxdepth)
	    :r-beg (widget-value elgrep-w-r-beg)
	    :r-end (widget-value elgrep-w-r-end)
	    :c-beg (let ((val (widget-value elgrep-w-c-beg)))
		     (if (numberp val) (- val) val))
	    :c-beg-only (widget-value elgrep-w-c-beg-only)
	    :c-end (widget-value elgrep-w-c-end)
	    :c-end-only (widget-value elgrep-w-c-end-only)
	    :case-fold-search (widget-value elgrep-w-case-fold-search)
	    :exclude-file-re (elgrep-widget-value-update-hist elgrep-w-exclude-file-re)
	    :dir-re (elgrep-widget-value-update-hist elgrep-w-dir-re)
	    :exclude-dir-re (elgrep-widget-value-update-hist elgrep-w-exclude-dir-re)
	    :interactive t
	    :async async
	    :elgrep-menu (and async elgrep-menu-id)
	    :buffer-init (widget-value elgrep-w-buffer-init)
	    :file-fun (widget-value elgrep-w-file-fun)
	    :search-fun (widget-value elgrep-w-search-fun))))

(defun elgrep-menu-elgrep-command (&rest _ignore)
  "Copy elgrep command resulting from current elgrep menu settings."
  (interactive "@")
  (let (print-level print-length)
    (kill-new (prin1-to-string (cons 'elgrep/i
				     (elgrep-menu-arg-list))))))

(defun elgrep-menu-check-elgrep-command (command)
  "Check whether COMMAND is a valid `elgrep' form.
COMMAND can be a string.  In that case the command is read from the string.
It can also be already a form.
If the car of COMMAND is a string then COMMAND is actually an elgrep call.
The car of the elgrep call is a name string
and the cdr is the actual elgrep command."
  (when (stringp command)
    (condition-case err
	(setq command (read command))
      (error (user-error "Reading command %S failed for the following reason: %S" command err))))
  (let (dir file-name-re re options (name ""))
    (unless (listp command)
      (user-error "Command \"%S\" must be a callable form" command))
    (when (stringp (car command))
      (setq name (car command)
	    command (cdr command)))
    (unless (memq (car command) '(elgrep elgrep/m elgrep/i))
      (user-error "Command \"%S\" is not elgrep" command))
    (when (< (length command) 4)
      (user-error "Not enough arguments to `elgrep' in command \"%S\"" command))
    (setq dir (nth 1 command)
	  file-name-re (nth 2 command)
	  re (nth 3 command)
	  options (nthcdr 4 command))
    (unless (stringp dir)
      (user-error "The directory name %S must be a string" dir))
    (unless (elgrep-string-or-function-p file-name-re)
      (user-error "The file name filter %S must be a regexp string or a filter function" file-name-re))
    (unless (or (stringp re)
		(and (listp re)
		     (cl-every #'stringp re)))
      (user-error "The regular expression %S must be a string or a list of strings" re))
    (cl-loop for option on options by #'cddr
	     for key = (car option)
	     for val = (cadr option)
	     for found = (assoc-string
			  (substring (symbol-name key) 1) ;;< Skip leading ?: of self-quoting symbols.
			  elgrep-menu-arg-alist)
	     for predicate = (cadr found)
	     unless found do (user-error "Unknown elgrep option key %S" key)
	     unless (or (eq predicate 'ignore)
			(funcall predicate val))
	     do (user-error "Wrong type of value %S for key %S" val key))
    (list dir file-name-re re options name)))

(defun elgrep-menu-set-from-command (command)
  "Set the `elgrep-menu' widgets from COMMAND.
COMMAND can be a form or a string containing
the printed representation of a form."
  (elgrep-menu-reset)
  (cl-destructuring-bind
      (dir file-name-re re options _name) (elgrep-menu-check-elgrep-command command)
    (widget-value-set elgrep-w-dir dir)
    (widget-value-set elgrep-w-file-name-re file-name-re)
    (widget-value-set elgrep-w-re re)
    (cl-loop for option on options by #'cddr
	     for opt-sym = (car option)
	     for predicate = (cadr (assoc-string (substring (symbol-name opt-sym) 1) elgrep-menu-arg-alist))
	     unless (eq predicate 'ignore) do
	     (cl-loop
	      unless (symbolp opt-sym) do (user-error "Not an elgrep option: %S" opt-sym)
	      for wid-sym = (intern-soft (concat "elgrep-w-" (substring (symbol-name opt-sym) 1)))
	      unless wid-sym do (user-error "Unknown elgrep option key :%S" (car option))
	      for wid = (symbol-value wid-sym)
	      do (widget-value-set wid (cadr option))
	      return nil)))
  (widget-setup))

(defun elgrep-menu-yank-elgrep-command (&rest _ignore)
  "Parametrize the `elgrep-menu' with the elgrep command from `kill-ring'."
  (interactive "@")
  (unless (derived-mode-p 'elgrep-menu-mode)
    (user-error "Buffer %S is not an elgrep-menu" (current-buffer)))
  (elgrep-menu-set-from-command (current-kill 0)))

(defvar elgrep-menu-button-map
  (let ((map (copy-keymap widget-keymap)))
    (define-key map [mouse-1] #'widget-button-click)
    map)
  "Keymap used for buttons widgets.
Also calls widget :action on clicking the first mouse button.")

(defvar-local elgrep-menu-hist-pos nil
  "Current position in text widget history.
Used in `elgrep-menu-hist-up' and `elgrep-menu-hist-down'.")

(let (hist) ;; We exploit lexical binding here!
  (defun elgrep-menu-hist-move (dir)
    "Move in :prompt-history of widget at point.
The direction is DIR which can have the values -1 or +1."
    (when-let ((wid (widget-at))
               (histvar (widget-get wid :prompt-history)))
      (unless hist (setq hist (cons (widget-value wid) (symbol-value histvar))))
      (unless (memq last-command '(elgrep-menu-hist-up elgrep-menu-hist-down))
        (setq hist (cons (widget-value wid) (symbol-value histvar)))
        (setq elgrep-menu-hist-pos 0))
      (let ((start elgrep-menu-hist-pos))
        (while
            (progn
              (setq elgrep-menu-hist-pos (mod (+ elgrep-menu-hist-pos dir) (length hist)))
              (condition-case nil
                  (progn
                    (widget-value-set wid (nth elgrep-menu-hist-pos hist))
                    nil)
                (error (/= elgrep-menu-hist-pos start)))))))))

(defun elgrep-menu-hist-up ()
  "Choose next item in :prompt-history of widget at point."
  (interactive)
  (elgrep-menu-hist-move 1))

(defun elgrep-menu-hist-down ()
  "Choose next item in :prompt-history of widget at point."
  (interactive)
  (elgrep-menu-hist-move -1))

(defvar elgrep-menu-hist-map (let ((map (copy-keymap widget-field-keymap)))
			       (define-key map (kbd "<M-up>") #'elgrep-menu-hist-up)
                               (define-key map (kbd "ESC <up>") #'elgrep-menu-hist-up)
			       (define-key map (kbd "<M-down>") #'elgrep-menu-hist-down)
                               (define-key map (kbd "ESC <down>") #'elgrep-menu-hist-down)
			       (define-key map (kbd "C-c <delete>") #'elgrep-widget-kill-content)
			       map)
  "Widget menu used for text widgets with history.
Binds M-up and M-down to one step in history up and down, respectively.")

(defun elgrep-widget-replace (old &rest args)
  "Replace widget OLD with widget created by `widget-create' applied to ARGS.
OLD can be a widget or the symbol with the old widget as value.
If OLD is a symbol that symbol is set to the newly created widget.
If the value of OLD is nil no old widget is deleted."
  (let ((wid (or (and (symbolp old) (symbol-value old))
		 old)))
    (when (widgetp wid)
      (goto-char (widget-get wid :from))
      (widget-delete wid))
    (setq wid (apply #'widget-create args))
    (when (symbolp old)
      (set old wid))))

(defun elgrep-widget-kill-content (point &optional copy-as-kill)
  "Kill content of widget at POINT.
Copy region if COPY-AS-KILL is non-nil."
  (interactive "d\nP")
  (if-let* ((wid (widget-at point))
	    (b (widget-field-start wid))
	    (e (widget-field-end wid)))
      (if copy-as-kill
	  (copy-region-as-kill b e)
	(kill-region b e))
    (message "Point not in text field")))

(defun elgrep-wid-dir-to-internal (_wid value)
  "Assert that the value of WID is a dir and return VALUE."
  (cl-assert (and (stringp value)
                  (file-directory-p value))
             nil
             "The value %S must be a directory" value)
  value)

(define-widget 'elgrep-re-widget 'editable-field
  "Widget type for specifying regular expressions."
  :tag "Regexp"
  :prompt-history 'elgrep-re-hist
  :keymap elgrep-menu-hist-map
  :format "%t: %v"
  "")

(defun elgrep-widget-elisp-completions (widget)
  "Run like `widget-default-completions' on WIDGET."
  (let ((b (widget-field-start widget)))
    (if (save-excursion
	  (goto-char b)
	  (looking-at-p "[[:space:]]*("))
	(elisp-completion-at-point)
      (list b (point) obarray
	    :predicate #'fboundp))))

(define-widget 'elgrep-invisible-const-widget 'const
  "An invisible constant widget."
  :format "")

(define-widget 'elgrep-elisp-widget 'sexp
  "Widget for elisp input as function or lisp form."
  :tag "Function or Elisp Form"
  :completions-function #'elgrep-widget-elisp-completions)

(define-widget 'elgrep-record-widget 'menu-choice
  "Widget type for `elgrep-w-r-beg' and `elgrep-w-r-end'."
  :args '((regexp :tag "Regexp")
	  (elgrep-elisp-widget)))

(defun elgrep-record-list-notify (this &rest rest)
  "Control activity of record end widget depending on THIS.
If the record begin widget is a list deactivate the end widget
and activate it otherwise.
Call `widget-default-notify' with THIS and REST."
  (let ((end (widget-get this :elgrep-record-end-widget)))
    (cl-assert end "Internal error record end not registered")
    (if (eq (widget-type (widget-get this :choice)) 'elgrep-record-widget)
	(widget-apply end :activate)
      (widget-apply end :deactivate)))
  (apply #'widget-default-notify this rest))

(define-widget 'elgrep-record-list-widget 'menu-choice
  "Let the user choose between simple record and nested records."
  :args '((cons :tag "Nested" :format "%t%v"
		(elgrep-invisible-const-widget :tag "" :value t)
		(repeat :tag ""
			(list
			 (elgrep-record-widget :tag "Record Begin")
			 (elgrep-record-widget :tag "Record End"))))
	  (elgrep-record-widget :tag "Begin"))
	  :notify #'elgrep-record-list-notify
	  :elgrep-record-end-widget nil)

(define-widget 'elgrep-regexp-or-function-widget 'menu-choice
  "Widget type offering a regexp or a function."
  :value "" :args '((regexp :tag "Regexp")
		   (function :tag "Function")))

(define-widget 'elgrep-context-widget 'menu-choice
  "Widget type for `elgrep-w-c-beg' and `elgrep-w-c-end'."
  :value 0
  :tag "Boundary"
  :args '((number :tag "Count")
	  (regexp :tag "Regexp")
	  (elgrep-elisp-widget)))

(defun elgrep-button-help-echo (wid)
  "Return help echo for button widget WID."
  (let ((doc (documentation (widget-get wid :action))))
    (format (substitute-command-keys "\\<elgrep-menu-button-map>\\[widget-button-click], \\[widget-button-press]: %s")
	    (substring doc nil (cl-position ?\n doc)))))

(define-widget 'elgrep-push-button 'push-button
  "Like widget type 'button but with keymap `elgrep-menu-button-map'."
  :keymap elgrep-menu-button-map
  :help-echo #'elgrep-button-help-echo)

(defun elgrep-reset-start-button (&optional id)
  "Insert a start button in elgrep menu with ID."
  (let ((buffer (or (and id (elgrep-get-menu-buffer id))
		    (current-buffer))))
    (with-current-buffer buffer
      (elgrep-widget-replace 'elgrep-w-start 'elgrep-push-button :value "Start elgrep" :action #'elgrep-menu-elgrep))))

(define-derived-mode elgrep-menu-mode fundamental-mode "Elgrep-Menu"
  "Major mode for elgrep menus."
  (add-hook 'write-contents-functions #'elgrep-save-elgrep-data-file nil t))

(defun elgrep-get-menu-buffer (id)
  "Get menu buffer with value of variable `elgrep-menu-id' equal to ID."
  (cl-loop for buf being the buffers
	   when (with-current-buffer buf
		  (and (derived-mode-p 'elgrep-menu-mode)
		       (eq elgrep-menu-id id)))
	   return buf))

(defun elgrep-true (&rest _ignore)
  "Ignore arguments and return t."
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elgrep-menu-call-list
(define-widget 'elgrep-menu-call-cut-button 'push-button
  "Cut button widget for elgrep."
  :tag "CUT"
  :help-echo "Copy this entry to the clipboard and delete it."
  :action 'elgrep-menu-call-cut-button-action)

(define-widget 'elgrep-menu-call-copy-button 'push-button
  "Copy button widget for elgrep."
  :tag "COPY"
  :help-echo "Copy this entry to the clipboard."
  :action 'elgrep-menu-call-copy-button-action)

(define-widget 'elgrep-menu-call-overwrite-button 'push-button
  "Overwrite button widget for elgrep."
  :tag "OVERWRITE"
  :help-echo "Overwrite this entry with the data from clipboard."
  :action 'elgrep-menu-call-overwrite-button-action)

(define-widget 'elgrep-menu-call-paste-button 'push-button
  "Cut button widget for elgrep."
  :tag "PASTE"
  :help-echo "Paste data from clipboard into a new entry before this one."
  :action 'elgrep-menu-call-paste-button-action)

(define-widget 'elgrep-menu-call-set-button 'push-button
  "Run button widget for elgrep."
  :tag "SET"
  :help-echo "Fill the elgrep menu with the settings of this entry."
  :action 'elgrep-menu-call-set-button-action)

(define-widget 'elgrep-menu-call-run-button 'push-button
  "Run button widget for elgrep."
  :tag "RUN"
  :help-echo "Run the elgrep command of this entry."
  :action 'elgrep-menu-call-run-button-action)

(define-widget 'elgrep-menu-call-show-code-checkbox 'checkbox
  "Toggle button for hiding code of call entry."
  :tag "Hide"
  :help-echo "Hide Elisp code for Elgrep call."
  :elgrep-code-visibility (lambda (widget show)
			    (widget-value-set widget show))
  :notify 'elgrep-menu-call-show-code-notify
  :value t)

(defconst elgrep-menu-call-list-button-alist
  '((?✂ . elgrep-menu-call-cut-button)
    (?∥ . elgrep-menu-call-copy-button)
    (?• . elgrep-menu-call-paste-button)
    (?␡ . elgrep-menu-call-overwrite-button)
    (?↓ . elgrep-menu-call-set-button)
    (?▶ . elgrep-menu-call-run-button)
    (?i . insert-button)
    (?d . delete-button)
    (?□ . elgrep-menu-call-show-code-checkbox))
  "Alist mapping widget format characters to widget types.")

(defun elgrep-menu-call-notify (widget changed &optional _event)
  "Set `elgrep-call-list' item if subwidget CHANGED of WIDGET changed."
  (if (eq widget changed) ;; list structure changed
      (setq elgrep-call-list (widget-value widget))
    (let ((index (widget-get changed :index)))
      (when (numberp index)
	(if (>= index (length elgrep-call-list)) ;; something is wrong... reinitialize
	    (setq elgrep-call-list (widget-value widget))
	  (setf (nth index elgrep-call-list)
		(widget-value changed))
	  )))))

(defun elgrep-menu-call-sexp-validate (widget)
  "Call `widget-sexp-validate' on WIDGET but don't error out.
Set the help-message instead to the error property of the widget."
  (if (widget-sexp-validate widget)
      (let ((err (widget-get widget :error)))
	(widget-put widget :help-echo
		    (format "Input sexp (%s)" err))
	(widget-put widget :elgrep-sexp-error
		    err)
	(widget-put widget :error nil))
    (widget-put widget :help-echo
		"Input sexp")
    (widget-put widget :elgrep-sexp-error nil))
  nil)

(defun elgrep-menu-call-sexp-value-to-external (widget value)
  "Return internal VALUE of WIDGET in the error case."
  (widget-apply widget :validate)
  (if (widget-get widget :elgrep-sexp-error)
      value ;; return as string
    (read value)))

(define-widget 'elgrep-menu-call-sexp 'sexp
  "Like sexp but don't error out.
Set the widget value to the string instead
and set help-echo to the error message."
  :validate #'elgrep-menu-call-sexp-validate
  :elgrep-code-visibility #'elgrep-widget-set-visibility ;; end point of call tree
  :value-to-internal #'elgrep-menu-call-sexp-value-to-internal
  :value-to-external #'elgrep-menu-call-sexp-value-to-external)

(defun elgrep-widget-apply-to-children (widget property &rest args)
  "Apply value of PROPERTY to children of WIDGET if PROPERTY is set there.
For each of the children PROPERTY is called with ARGS."
  (let (fun)
    (cl-loop
     for child in (widget-get widget :children)
     when (functionp (setq fun (widget-get child property)))
     do (apply fun child args))))

(defun elgrep-widget-set-visibility (widget show)
  "Set visibility of WIDGET according to SHOW."
  (let ((overlay (widget-get widget :invisible)))
    (if show
	(when (overlayp overlay)
	  (delete-overlay overlay)
	  (widget-put widget :invisible nil))
      ;; hide
      (unless (overlayp overlay)
	(setq overlay
	      (make-overlay
	       (let ((from (widget-get widget :from))
		     beg)
		 (if (save-excursion
		       (goto-char from)
		       (setq beg (line-beginning-position))
		       (looking-back "^[[:space:]]*" beg))
		     beg
		   from))
	       (let ((to (widget-get widget :to)))
		 (if (save-excursion
		       (goto-char to)
		       (looking-at "[[:space:]]*$"))
		     (line-beginning-position 2)
		   to))
	       nil t nil))
	(overlay-put overlay 'invisible t)
	(overlay-put overlay 'evaporate t)
	(widget-put widget :invisible overlay))
      )))

(defsubst elgrep-widget-default-code-visibility (widget show)
  "Set code visibility of WIDGET's children according to SHOW."
  (elgrep-widget-apply-to-children widget :elgrep-code-visibility show))

(define-widget 'elgrep-menu-call-list-entry 'cons
  "Menu call list entry."
  :tag "Elgrep Call"
  :elgrep-code-visibility #'elgrep-widget-default-code-visibility
  :args '((string :tag "Name")
	  (elgrep-menu-call-sexp :tag "Form" :value nil))
  '("" . nil))

(define-widget 'elgrep-menu-call-list 'editable-list
  "Like `editable-list' widget with a name string and an elgrep form."
  :format "%v%i %•\n"
  :entry-format "%i %d %∥ %␡ %• %↓ %▶ Code:%□ %v"
  :format-handler #'elgrep-menu-call-list-format-handler
  :value-create #'elgrep-menu-call-list-value-create
  :insert-before #'elgrep-menu-call-list-insert-before
  :delete-at #'elgrep-menu-call-list-delete-at
  :notify #'elgrep-menu-call-notify
  :elgrep-code-visibility #'elgrep-widget-default-code-visibility)

(defun elgrep-menu-call-list-format-handler (widget escape)
  "Handle :format of WIDGET `elgrep-menu-call-list' for char ESCAPE."
  (let ((wid-type (cdr (assoc escape elgrep-menu-call-list-button-alist))))
    (if wid-type
	(progn
	  (and (widget-get widget :indent)
	       (insert-char ?\s (widget-get widget :indent)))
	  (apply 'widget-create-child-and-convert
		 widget wid-type
		 (widget-get widget :append-button-args)))
      (widget-default-format-handler widget escape))))

(defun elgrep-menu-call-list-entry-create (widget value conv)
  "Insert also USE, UP, and DOWN buttons in WIDGET.
VALUE and CONV are used in `widget-editable-list-entry-create'."
  (let ((type (nth 0 (widget-get widget :args)))
	buttons child)
    (widget-specify-insert
     (save-excursion
       (when (widget-get widget :indent)
	 (insert-char ?\s (widget-get widget :indent)))
       (insert (widget-get widget :entry-format)))
     (while (re-search-forward "%\\(.\\)" nil t)
       (let* ((escape (char-after (match-beginning 1)))
	      (wid-type (cdr (assoc escape elgrep-menu-call-list-button-alist))))
	 (cond
	  (wid-type
	   (delete-char -2)
	   (let ((button (apply 'widget-create-child-and-convert
				widget wid-type
				(widget-get widget :append-button-args))))
	     (push button buttons)))
	  ((eq escape ?v)
	   (delete-char -2)
	   (if conv
	       (setq child (widget-create-child-value
			    widget type value))
	     (setq child (widget-create-child-value
			  widget type (widget-default-get type)))))
	  (t
	   (error "Unknown escape `%c'" escape))
	  )))
     (widget-put widget :buttons
		 (cl-union (widget-get widget :buttons) buttons))
     (let ((entry-from (point-min-marker))
	   (entry-to (point-max-marker)))
       (set-marker-insertion-type entry-from t)
       (set-marker-insertion-type entry-to nil)
       (widget-put child :entry-from entry-from)
       (widget-put child :entry-to entry-to)))
    (dolist (button buttons)
      (widget-put button :widget child))
    child))

(defun elgrep-menu-call-list-renumber (widget)
  "Re-index the children of WIDGET.
Afterwards the children are consecutively numbered
by the :index property."
  (cl-loop
   for child in (widget-get widget :children)
   for i from 0 do
   (widget-put child :index i)))

(defun elgrep-menu-call-list-value-create (widget)
  "Create the buffer representation WIDGET from its value."
  (let* ((value (widget-get widget :value))
	 (type (nth 0 (widget-get widget :args)))
	 children)
    (widget-put widget :value-pos (point-marker))
    (set-marker-insertion-type (widget-get widget :value-pos) t)
    (while value
      (let ((answer (widget-match-inline type value)))
	(if answer
	    (setq children (cons (elgrep-menu-call-list-entry-create
				  widget
				  (if (widget-get type :inline)
				      (car answer)
				    (car (car answer)))
				  t)
				 children)
		  value (cdr answer))
	  (setq value nil))))
    (widget-put widget :children (nreverse children)))
  (elgrep-menu-call-list-renumber widget)
  (setq elgrep-call-list (widget-value widget)))

(defun elgrep-menu-call-list-insert-before (widget before)
  "Insert a new child before the child widget BEFORE of WIDGET."
  (let (child)
    (save-excursion
      (let ((children (widget-get widget :children))
	    (inhibit-read-only t)
	    (inhibit-modification-hooks t))
	(cond (before
	       (goto-char (widget-get before :entry-from)))
	      (t
	       (goto-char (widget-get widget :value-pos))))
	(setq child (elgrep-menu-call-list-entry-create
		     widget nil nil))
	(when (< (widget-get child :entry-from) (widget-get widget :from))
	  (set-marker (widget-get widget :from)
		      (widget-get child :entry-from)))
	(if (eq (car children) before)
	    (widget-put widget :children (cons child children))
	  (while (not (eq (car (cdr children)) before))
	    (setq children (cdr children)))
	  (setcdr children (cons child (cdr children))))))
    (widget-setup)
    (widget-apply widget :notify widget)
    (elgrep-menu-call-list-renumber widget)
    child))

(defmacro elgrep-remove--at-macro (list index &rest copy)
  "Helper for defining `elgrep-remove-at' and `elgrep-remove-at*'."
  (let ((tail (make-symbol "tail")))
    `(cond
      ((eq ,index 0)
       (cdr ,list))
      ((> ,index (length ,list))
       ,list)
      (t
       ,@copy
       (let ((,tail (nthcdr (1- ,index) ,list)))
	 (setcdr ,tail (cddr ,tail))
	 ,list)))))

(defun elgrep-remove-at* (list index)
  "Remove element at INDEX from LIST.
Noop if INDEX is larger than length of LIST.
The list is modified by side-effect.
Return the modified list."
  (elgrep-remove--at-macro list index))
;; tests:
;; (let ((l '(0 1 2 3))) (vector (elgrep-remove-at* l 2) l))
;; (let ((l '(0 1 2 3))) (vector (elgrep-remove-at* l 3) l))
;; (let ((l '(0 1 2 3))) (vector (elgrep-remove-at* l 0) l))
;; (let ((l '(0 1 2 3))) (vector (elgrep-remove-at* l 4) l))

(defun elgrep-remove-at (list index)
  "Remove element at INDEX from LIST.
Noop if INDEX is larger than length of LIST.
The original list is not modified.  It is copied if needed.
Return the modified list."
  (elgrep-remove--at-macro list index (setq list (cl-copy-list list))))
;; tests:
;; (let ((l '(0 1 2 3))) (vector (elgrep-remove-at l 2) l))
;; (let ((l '(0 1 2 3))) (vector (elgrep-remove-at l 3) l))
;; (let ((l '(0 1 2 3))) (vector (elgrep-remove-at l 0) l))
;; (let ((l '(0 1 2 3))) (vector (elgrep-remove-at l 4) l))

(defun elgrep-menu-call-list-delete-at (widget child)
  "Delete CHILD of WIDGET.
Essentially use `widget-editable-list-delete-at' but also
update `elgrep-call-list'."
  (let ((index (widget-get child :index)))
    (setq elgrep-call-list (elgrep-remove-at elgrep-call-list index))
    (widget-editable-list-delete-at widget child)))

(defun elgrep-menu-call-cut-button-action (button &optional _event)
  "Copy command of BUTTON widget to clipboard and deleting it."
  (elgrep-menu-call-copy-button-action button)
  (widget-delete-button-action button))

(defun elgrep-menu-call-copy-button-action (button &optional _event)
  "Copy command of BUTTON widget to clipboard."
  (let* (print-level
	 print-length
	 (widget (widget-get button :widget))
	 (command (widget-value widget)))
    (kill-new (format "%S" command))))

(defun elgrep-menu-call-sexp-value-to-internal (_widget value)
  "Don't insert newlines for VALUE.
Otherwise work like `widget-sexp-value-to-internal'."
  (let* (print-level
	 print-length
	 (pp (if (symbolp value)
		 (prin1-to-string value)
	       (pp-to-string value))))
    (while (string-match "\n\\'" pp)
      (setq pp (substring pp 0 -1)))
    pp))

(defun elgrep-menu-call-overwrite-button-action (button &optional _event)
  "Overwrite BUTTON widget value with command from clipboard."
  (let* ((widget (widget-get button :widget))
	 (idx (widget-get widget :index))
	 (command (current-kill 0)))
    (cl-destructuring-bind
	(dir file-name-re re options name) (elgrep-menu-check-elgrep-command command)
      (setq command (cons name  `(elgrep ,dir ,file-name-re ,re ,@options)))
      (widget-value-set widget command)
      (setf (nth idx elgrep-call-list) command))
    (widget-setup)))

(defun elgrep-menu-call-paste-button-action (button &optional _event)
  "Insert clipboard data as widget before BUTTON widget."
  (let* ((widget (widget-insert-button-action button))
	 (parent (widget-get widget :parent))
	 (button (cl-loop for but in (widget-get parent :buttons)
			  if (eq (widget-get but :widget) widget)
			  return but)))
    (elgrep-menu-call-overwrite-button-action button)))

(defvar elgrep-widget-button-click-moves-point nil
  "Only certain buttons move point.
Cache for `widget-button-click-moves-point'.")

(defun elgrep-widget-restore-widget-button-click-moves-point ()
  "Restore the value of `elgrep-widget-button-click-moves-point'.
Done in commands like `elgrep-menu-call-set-button-action' that
want to move point back to the menu."
  (remove-hook 'post-command-hook #'elgrep-widget-restore-widget-button-click-moves-point t)
  (setq widget-button-click-moves-point elgrep-widget-button-click-moves-point
	elgrep-widget-button-click-moves-point nil))

(defun elgrep-menu-call-set-button-action (button &optional _event)
  "Run elgrep for the associated `elgrep-menu-call-list' entry of BUTTON."
  (if-let ((widget (widget-get button :widget))
	   (command (cdr (widget-value widget))))
      (progn
	(elgrep-menu-set-from-command command)
	(setq elgrep-widget-button-click-moves-point widget-button-click-moves-point
	      widget-button-click-moves-point t)
	(add-hook 'post-command-hook #'elgrep-widget-restore-widget-button-click-moves-point nil t))
    (error "Set button action failed; command:%s" command)))

(defun elgrep-menu-call-run-button-action (button &optional _event)
  "Run elgrep for the associated `elgrep-menu-call-list' entry of BUTTON."
  (if-let ((widget (widget-get button :widget))
	   (command (cdr (widget-value widget))))
      (progn
	(elgrep-menu-check-elgrep-command command)
	(apply #'elgrep (append (cdr command) '(:interactive t))))
    (error "Run button action failed; command:%s" command)))

(defun elgrep-menu-call-show-code-notify (checkbox changed &optional _event)
  "Hide code of menu call corresponding to WIDGET.
EVENT is passed to `widget-checkbox-action'."
  (when (eq checkbox changed)
    (let* ((widget (widget-get checkbox :widget))
	   (show (widget-value checkbox)))
      (elgrep-widget-default-code-visibility widget show))))

(defun elgrep-menu-call-add-to-list (widget command)
  "Write elgrep COMMAND to list WIDGET.
If the first command in WIDGET is unnamed replace that one.
Otherwise add a unnamed command at the top of WIDGET."
  (let ((first (car (widget-get widget :children))))
    (unless
	(and first
	     (string-empty-p (car (widget-value first))))
      (setq first (elgrep-menu-call-list-insert-before widget first)))
    (widget-value-set first (cons "" command))
    (widget-apply widget :notify first) ;; Should already be done by `widget-value-set'.
    ;; I consider that a bug of the widget library.
    )
  (widget-setup))

(defun elgrep-menu-call-list-show-code (show)
  "Set visibility of code according to SHOW in all call list entries."
  (elgrep-widget-default-code-visibility elgrep-w-call-list show)
  (when-let ((buttons (widget-get elgrep-w-call-list :buttons)))
    (dolist (button buttons)
      (let ((fun (widget-get button :elgrep-code-visibility)))
	(when (functionp fun)
	  (funcall fun button show))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun elgrep-menu (&optional reset)
  "Present a menu with most of the parameters for `elgrep'.
Reset the menu entries if RESET is non-nil.
You can adjust the parameters there and start `elgrep'."
  (interactive "P")
  (unless
      (prog1 (and (buffer-live-p (get-buffer "*elgrep-menu*"))
		  (null reset))
	(switch-to-buffer "*elgrep-menu*"))
    (elgrep-menu-mode)
    (setq-local elgrep-menu-id elgrep-menu-id)
    (cl-incf (default-value 'elgrep-menu-id))
    (setq default-directory
	  (or
	   (cl-loop for buf in (buffer-list)
		    if (or
			(buffer-file-name buf)
			(with-current-buffer buf
			  (derived-mode-p 'dired-mode 'eshell-mode)))
		    return (with-current-buffer buf default-directory))
	   default-directory))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (buffer-disable-undo)
    (let ((caption "Elgrep Menu"))
      (widget-insert (concat caption "
" (make-string (length caption) ?=) "

Hint: Try <M-tab> for completion, and <M-up>/<M-down> for history access.

")))
    (setq-local elgrep-w-re (elgrep-widget-create 'menu-choice
						  :tag "Expression"
						  :help-echo "Regexp or list of regexps"
						  :value ""
						  :prompt-history 'elgrep-re-hist
						  '(repeat
						    :tag "List of regexps"
						    :default-get (lambda (wid) (list ""))
						    elgrep-re-widget
						    )
						  'elgrep-re-widget
						  ))
    (setq-local elgrep-w-dir (widget-create 'directory
					    :prompt-history 'file-name-history
					    :keymap elgrep-menu-hist-map
					    :value-to-internal #'elgrep-wid-dir-to-internal
					    :format "Directory: %v" default-directory))
    (setq-local elgrep-w-file-name-re (elgrep-widget-create 'elgrep-regexp-or-function-widget
							    :prompt-history 'elgrep-file-name-re-hist
							    :keymap elgrep-menu-hist-map
							    :tag "File Name Filter (Regexp or Filter Function): " :value (elgrep-default-filename-regexp default-directory)))
    (setq-local elgrep-w-exclude-file-re (elgrep-widget-create 'elgrep-regexp-or-function-widget
							       :prompt-history 'elgrep-exclude-file-re-history
							       :keymap elgrep-menu-hist-map
							       :tag "Exclude File Name Regexp or Filter (ignored when empty)"
							       :value ""))
    (setq-local elgrep-w-dir-re (elgrep-widget-create 'regexp
						      :prompt-history 'regexp-history
						      :keymap elgrep-menu-hist-map
						      :format "Directory Name Regular Expression: %v" ""))
    (setq-local elgrep-w-exclude-dir-re (elgrep-widget-create 'regexp
							      :prompt-history 'regexp-history
							      :keymap elgrep-menu-hist-map
							      :format "Exclude Directory Name Regular Expression (ignored when empty): %v" ""))
    (widget-insert  "Recurse Into Subdirectories ")
    (setq-local elgrep-w-recursive (elgrep-widget-create 'checkbox nil))
    (widget-insert  "  Follow Symlinks ")
    (setq-local elgrep-w-symlink (elgrep-widget-create 'checkbox nil))
    (setq-local elgrep-w-async (elgrep-widget-create
				'(radio-button-choice :tag "\nRun Asynchronously (experimental)" :format "%t: %v"
					 (const :tag "Separate instance of Emacs" :format "%t\t" t)
					 (const :tag "Separate thread" :format "%t\t" thread)
					 (const :tag "Synchronous" nil))))
    (setq-local elgrep-w-mindepth (elgrep-widget-create 'number :format "Minimal Recursion Depth: %v" 0))
    (setq-local elgrep-w-maxdepth (elgrep-widget-create 'number :format "Maximal Recursion Depth: %v" most-positive-fixnum))
    (setq-local elgrep-w-r-beg (elgrep-widget-create 'elgrep-record-list-widget :tag "Record" :value '(point-min)))
    (setq-local elgrep-w-r-end (elgrep-widget-create 'elgrep-record-widget :tag "Record End" :value '(point-max)))
    (widget-put elgrep-w-r-beg :elgrep-record-end-widget elgrep-w-r-end)
    (widget-insert "Context before match: ")
    (setq-local elgrep-w-c-beg-only (elgrep-widget-create '(menu-choice :value nil :format "%[%v%]" (item :tag "Line" :format "%t" nil) (item :tag "Character" :format "%t" t))))
    (widget-insert " ")
    (setq-local elgrep-w-c-beg (elgrep-widget-create 'elgrep-context-widget))
    (widget-insert "Context after match: ")
    (setq-local elgrep-w-c-end-only (elgrep-widget-create '(menu-choice :value nil :format "%[%v%]" (item :tag "Line" :format "%t" nil) (item :tag "Character" :format "%t" t))))
    (widget-insert " ")
    (setq-local elgrep-w-c-end (elgrep-widget-create 'elgrep-context-widget))
    (setq-local elgrep-w-case-fold-search
		(elgrep-widget-create
		 '(choice :tag "Case Sensitivity" :format "%t: %[Options%] %v" :doc "Ignore case."
			  :value default
			  (const :tag "Default (Value of `case-fold-search')" default)
			  (const :tag "Case Insensitive Search" t)
			  (const :tag "Case Sensitive Search" nil))))
    (setq-local elgrep-w-buffer-init
		(elgrep-widget-create
		 '(choice :tag "Buffer initialization" :format "%t: %[Options%] %v" :help-echo "Initialization of the buffer where the file is read in and searched."
			  :value nil
			  (const :tag "No initialization" :value nil)
			  (const :tag "Set syntax table" :value syntax-table)
			  (const :tag "Full major mode initialization" :value major-mode))))
    (setq-local elgrep-w-file-fun (elgrep-widget-create 'function :format "File predicate function: %v" :help-echo "Function taking the file or directory path as argument. Should return non-nil if that path should be included in the search." #'elgrep-true))
    (setq-local elgrep-w-search-fun (elgrep-widget-create 'function :format "Search function: %v " :help-echo "Search function called with the first three arguments of `re-search-forward': 1st the expression, 2nd BOUND, 3rd NOERROR" #'re-search-forward))
    (widget-insert "\n")
    (elgrep-widget-replace 'elgrep-w-start 'elgrep-push-button :value "Start elgrep" :action #'elgrep-menu-elgrep)
    (widget-insert " ")
    (widget-create 'elgrep-push-button :value "Copy" :action #'elgrep-menu-elgrep-command)
    (widget-insert " ")
    (widget-create 'elgrep-push-button :value "Paste" :action #'elgrep-menu-yank-elgrep-command)
    (widget-insert " ")
    (widget-create 'elgrep-push-button :value "Burry" :action (lambda (&rest _ignore) "Burry elgrep menu." (interactive "@") (bury-buffer)))
    (widget-insert " ")
    (widget-create 'elgrep-push-button :value "Reset" :action (lambda (_widget event) "Reset elgrep menu." (interactive "@") (elgrep-menu event)))
    (widget-insert " ")
    (widget-create 'elgrep-push-button
		   :value "Show Code"
		   :action
		   (lambda (&rest _ignore)
		     "Show code of all entries."
		     (interactive "@")
		     (elgrep-menu-call-list-show-code t)))
    (widget-insert " ")
    (widget-create 'elgrep-push-button
		   :value "Hide Code"
		   :action
		   (lambda (&rest _ignore)
		     "Hide code of all entries."
		     (interactive "@")
		     (elgrep-menu-call-list-show-code nil)))
    (use-local-map widget-keymap)
    (local-set-key "q" #'bury-buffer)
    (widget-insert (propertize (concat "\n" (make-string 70 ?_) "\nElgrep call list:\n")
			       'help-echo "If the first elgrep call is unnamed it is updated by the next call of elgrep.
Otherwise a new elgrep call is added."))
    (setq-local elgrep-w-call-list (widget-create '(elgrep-menu-call-list
						    (elgrep-menu-call-list-entry))
						  :value elgrep-call-list))
    (widget-setup)
    (set-window-start (selected-window) (point-min))
    (when-let ((choice (car (widget-get elgrep-w-re :children)))
	       (re (or (car-safe (widget-get choice :children)) choice)))
      (goto-char (widget-field-start re)))
    (local-set-key (kbd "C-c C-c") #'elgrep-menu-elgrep)
    (buffer-enable-undo)))

(defun elgrep-get-formatter ()
  "Return a formatter for elgrep-lines.
The formatter is a function with two arguments FNAME and PARTS.
FNAME is the file name where the match occurs.
PARTS is a list of parts.
Each PART is a property list with members

:match (the actual match)

:context (the match including context lines)

:line (the line in the source code file)

:line-beg (the beginning position of the context in the source code file)

:beg (the beginning position of the match)

:end (the end position of the match)

The formatter is actually a capture
that remembers the last file name and the line number
such that the same line number is not output multiple times."
  (let ((last-file "")
	(last-line 0)
	(output-beg 0))
    (lambda (fname parts)
      (when (consp parts)
	(when (buffer-live-p fname)
	  (setq fname (buffer-file-name fname)))
	(let* ((part (car parts))
	       (line (plist-get part :line)))
	  (unless (and (string-equal last-file fname)
		       (= last-line line))
	    (insert (propertize (format "%s:%d:" fname line)
				'elgrep 'context
				'elgrep-context-begin (plist-get part :context-beg)
				'elgrep-context-end (plist-get part :context-end)
				'elgrep-context (plist-get part :context)
				))
	    (setq output-beg (point))
	    (insert (plist-get part :context) ?\n))
	  (let ((context-beg (plist-get part :context-beg)))
	    (cl-loop for part in parts do
		     (let ((match-beg (+ (- (plist-get part :beg) context-beg) output-beg))
			   (match-end (+ (- (plist-get part :end) context-beg) output-beg)))
		       (when (and (>= match-beg output-beg)
				  (<= match-end (point-max)))
			 (put-text-property match-beg match-end 'font-lock-face 'match)))))
	  (setq last-file fname
		last-line line)
	  )))))

(defvar compilation-last-buffer) ; defined in "compile.el"

(defun elgrep-list-matches (filematches &rest options)
  "Insert FILEMATCHES as returned by `elgrep' in current buffer.
OPTIONS is a plist of options as for `elgrep'."
  (let ((opt-list (car-safe options)))
    (when (listp opt-list)
      (setq options opt-list)))
  (setq compilation-last-buffer (current-buffer))
  (unless (plist-get options :no-header)
    (insert
     (propertize
      (format "-*- mode: elgrep; default-directory: %S -*-\n" default-directory)
      'elgrep 'header
      )))
  (let ((formatter (or (plist-get options :formatter)
		       (elgrep-get-formatter))))
    (dolist (filematch filematches)
      (let ((fname (car filematch))
	    stack)
	(dolist (match (cdr filematch))
	  (let ((part (car-safe match)))
	    (if (or (null stack)
		    (eq (plist-get (car stack) :line) (plist-get part :line)))
		(push part stack)
	      (funcall formatter fname stack)
	      (setq stack (list part)))
	    ))
	(when stack
	  (funcall formatter fname stack))
	))))

(defun elgrep-dir-name (dir)
  "Expand DIR with substitution of environment variables."
  (if dir
      (expand-file-name (directory-file-name (substitute-in-file-name dir)))
    default-directory))

(defsubst elgrep-directory-files (directory &optional full match nosort)
  "Run `directory-files' protected by `condition-case'.
DIRECTORY, FULL, MATCH and NOSORT are the arguments of `directory-files'.
Windows can read-protect directories even if `file-accessible-directory-p'
returns t.

The references \".\" and \"..\" of the current directory and
the parent directory are filtered out from the results.

MATCH can also be a filter function taking a list of files as argument
and returning a filtered list of files.

Return nil if reading of the directory fails."
  (condition-case err
      (let* ((match-re (and (stringp match) match))
	     (files (cl-delete-if
		     (lambda (file)
		       (member file '("." "..")))
		     (funcall #'directory-files directory full match-re nosort))))
	(if (functionp match)
	    (funcall match files)
	  files))
    (file-error
     (elgrep-log-file-error err "Read error at directory %S" directory)
     nil)))

(defun elgrep-get-auto-mode ()
  "Get auto-mode via `set-auto-mode'.
`set-auto-mode-0' is adviced to just return the mode name."
  (cl-letf* ((mode-symbol nil)
	     ((symbol-function 'set-auto-mode-0) (lambda (mode &rest _)
						   (setq mode-symbol mode)))
	     ((symbol-function 'set-buffer-major-mode) (lambda (&rest _)
							 nil)))
    (set-auto-mode)
    mode-symbol))
;; test
;; (elgrep-get-auto-mode)

(defun elgrep-initialize-buffer (file options)
  "Insert FILE contents and set syntax table or mode according to OPTIONS."
  (erase-buffer)
  (elgrep-insert-file-contents (if (plist-get options :abs) file buffer-file-name))
  (cl-case (plist-get options :buffer-init)
    (syntax-table
     (when-let ((mode (elgrep-get-auto-mode))
		(table (intern-soft (concat (symbol-name mode) "-syntax-table"))))
       (set-syntax-table (symbol-value table))))
    (major-mode
     (after-find-file nil nil t))))

(defmacro elgrep-prepare-buffer (file dir options &rest body)
  "Prepare < *elgrep-search*> buffer for check of FILE in DIR with OPTIONS.
Run BODY like `progn'."
  (declare (indent 3) (debug (sexp sexp sexp body)))
  `(let ((buffer-file-name (expand-file-name ,file ,dir)))
     (elgrep-initialize-buffer ,file ,options)
     ,@body))

(defun elgrep--search-forward (search &optional match-beg)
  "Search for SEARCH.
If SEARCH is a regexp then search with `re-search-forward'.
If it is a function call that function without args.
It should return the position of the match if it finds one.
Otherwise emit error.
If MATCH-BEG is non-nil reset `match-data' go to the beginning of the match.
That is done by resetting the match data before running the search function
and going to `match-beginning' in that case that the search function sets
the match data.
The `match-data' is not reset if MATCH-BEG is nil.
The `match-data' of the search for r-beg can be used in the search for r-end."
  (when match-beg
    (set-match-data nil))
  (let ((ret
	 (cond
	  ((functionp search)
	   (funcall search))
	  ((stringp search)
	   (re-search-forward search nil 'noError))
	  (t (eval search)))))
    (if (and match-beg
	     (match-data))
	(match-beginning 0)
      ret)))

(defun elgrep-with-records-f (r-beg r-end fun)
  "Search buffer for records bounded by R-BEG, R-END and execute FUN therein.
This is the driver function for `elgrep-with-records'.
The record boundaries are searched with `elgrep--search-forward'."
  (let ((pt-min (point-min))
	(pt-max (point-max))
	b (e (1- (point-min))))
    (when (eq r-end t)
      (when (cdr r-beg)
	(let ((r-beg-new (cdr r-beg))
	      (old-fun fun))
	  (setq fun (lambda ()
		      (elgrep-with-records-f
		       r-beg-new
		       t
		       old-fun)))))
      (setq r-end (cadar r-beg)
	    r-beg (caar r-beg)))
    (while
	(when (and (setq b (elgrep--search-forward r-beg t))
		   (setq b (if (< e b) ;; Search for r-beg:"^" and r-end:"$" in "\n\n"
			       b        ;; finds the same position.
			     (and (< e (point-max))
				  (goto-char (1+ e))
				  (elgrep--search-forward r-beg t))))
		   (setq e (elgrep--search-forward r-end)))
	  (narrow-to-region b e)
	  (goto-char b)
	  (save-restriction
	    (funcall fun))
	  (widen) (narrow-to-region pt-min pt-max) ;; restore original region
	  (goto-char e)
	  (< e (point-max))))))

(defmacro elgrep-with-records (r-beg r-end &rest body)
  "Search buffer for records bounded by R-BEG, R-END and execute BODY therein.
The record boundaries are searched with `elgrep--search-forward'."
  (declare (indent 2) (debug (sexp sexp body)))
  `(elgrep-with-records-f
    ,r-beg
    ,r-end
    (lambda ()
      ,@body)))

(defun elgrep-intern-plist-keys (plist)
  "Intern all string keys of PLIST that are given.
Keys given as symbols are not touched.
This is a destructive operation."
  (cl-loop for key in-ref plist by #'cddr
	   if (stringp key)
	   do (setf key (intern key)))
  plist)
;; Test:
;; (equal (elgrep-intern-plist-keys (list ":first" 1 :second "2" ":third" 3)) '(:first 1 :second "2" :third 3))

(defun elgrep-args-options (&rest optional-args)
  "Get options from elgrep ARGS.
ARGS defaults to the value of `elgrep-args'.

Note, that ARGS is actually retrieved from (car OPTIONAL-ARGS).

Arglist of the actual implementation: &rest OPTIONAL-ARGS

\(fn &optional ARGS)"
  (let ((args (if optional-args
		  (car optional-args)
		elgrep-args)))
    (nthcdr 3 args)))

;;;###autoload
(defun elgrep (dir file-name-re re &rest options)
  "In path DIR grep files with name matching FILE-NAME-RE for text matching RE.
This is done via Emacs Lisp (no dependence on external grep).
Return list of filematches.

FILE-NAME-RE can be a regexp or a filter function taking a list of file names
and returning a filtered list of file names.

Each filematch is a cons (file . matchdata).
file is the file name.
matchdata is a list of matches.
Each match is a list of sub-matches.
Each submatch is a plist of :match, :context, :line,
:linestart, :beg and :end.

OPTIONS is a plist
Flags:

:abs absolute file names
t: full absolute file names;
nil: (default) file names relative to `default-directory'
of the last visited buffer

:interactive
t: call as interactive

:r-beg record begin
Beginning of next record.
Can be a regular expression, a function without args
or a list of record delimiters.
If the function finds a record beginning, it should return its position
like `search-forward'.
Search starts at buffer beginning or at end of last record.
Defaults to `point-min'.
A list of record delimiters allows to define nested records.
One example where this becomes handy is, when one wants to grep
for identifiers in org source blocks within certain sections of Org-files.
In that example the first record could start at a match of \"^\\* SECTION\"
and end at a match of \"^\\* \\|\\'\"
and the second record could be delimited by matches of
\" *#+begin_src\" and \" *#+end_src\".
A list of record delimiters is marked with the value t in its first element.
Starting with its cdr, it contains record delimiters.
Each record delimiter is a list.
The first element of that list is the regular expression or
the function matching the beginning of the record
and the second element of that list is the regular expression
or function matching the end of the record.
For the above example the `elgrep' command would look like:
\(elgrep ...
    :r-beg (t
            (\"^\\\\* W:22205\" \"^\\\\* \\\\|\\\\'\")
            (\" *#\\\\+begin_src\" \" *#\\\\+end_src \"))
    ...)

:r-end record end
End of record.
Can be a regular expression or a function without args.
If the function finds a record end it should return its position
like `search-forward'.
Search starts at search result for :r-beg.
Defaults to `point-max'.

:c-beg context begin (line beginning)
Lines before match defaults to 0. Can also be a regular expression.
Then this re is searched for in backward-direction
starting at the beginning of the current elgrep-match.
It can also be a function moving point to the context beginning
starting at the match of RE.

:c-end context end (line end)
Lines behind match defaults to 0. Can also be a regular expression.
Then this re is searched for in forward-direction
starting at the end of the current elgrep-match.
It can also be a function moving point to the context end
starting at the match of :c-beg.

:c-beg-only
Use the context beginning literally.
That means do not extend the context to the beginning of line.

:c-end-only
Use the context end literally.
That means do not extend the context to the end of line.

:c-op
Context operation gets beginning and end position of context as arguments.
Defaults to `buffer-substring-no-properties'.

:recursive
t: also grep recursively subdirectories in dir
\(also if called interactively with prefix arg)
Defaults to nil.

:symlink
t: also follow symbolic links when recursing

:formatter
Formatting function to call for each match
if called interactively with non-nil RE.
Inputs: format string \"%s:%d:%s\n\", file-name, line number,

:exclude-file-re
Regular expression matching the files that should not be grepped.
Do not exclude files if this option is nil, unset, or the empty string.
Can also be a filter function that gets the full list of file names
as argument and should return the filtered list to be used for `elgrep'.
Defaults to nil.

:dir-re
Regular expression matching the directories
that should be entered in recursive grep.
Defaults to \"\".

:exclude-dir-re
Regular expression matching the directories
that should not be entered in recursive grep.
If this is the empty string no directories are excluded.
Defaults to \"^\\.\".

:case-fold-search
Ignore case if non-nil.
Defaults to the value of `case-fold-search'.

:buffer-init may be one of the following symbols:
nil (default): Do not initialize buffer.
syntax-table: Just set the syntax table corresponding
              to the auto-mode of the file.
major-mode: Full major-mode initialization of the auto-mode corresponding
            to the file.

:file-fun
Predicate function called with the file path as argument.
The function should return non-nil if that file should be searched.
If the return value is a string it is used as new file name for `elgrep-save'.
Option :abs decides whether the path is relative or absolute.

:search-fun
Function to search forward for occurences of RE
with the same arguments as `re-search-forward'.
It gets RE as first argument.
Thereby it is not required that RE is a regular expression.
Defaults to `re-search-forward'.

:keep-elgrep-buffer
Keep buffer <*elgrep*> even when there are no matches.

:no-header
Avoid descriptive header into <*elgrep*> buffer.

:async
Asynchronous search (experimental).
Search synchronous if this option is nil,
search in a separate thread if this option is equal to 'thread,
and search with the help of the library async otherwise.

:mindepth Minimal depth. Defaults to 0.

:maxdepth Maximal depth. Defaults to the value of `most-positive-fixnum'.

:depth Internal. Should not be used."
  (interactive (elgrep-read-args 'files))
  (when (called-interactively-p 'any)
    (setq options (plist-put options :interactive t)))
  ;; make elgrep eshell friendly:
  (setq options (elgrep-intern-plist-keys options))
  (when (and (stringp re) (= (length re) 0))
    (setq re nil))
  (setq dir (elgrep-dir-name dir))
  (let ((async (plist-get options :async)))
    (cond
     ((eq async 'thread)
      (setq elgrep-thread
	    (make-thread
	     `(lambda ()
		(unwind-protect
		    (apply #'elgrep-show (apply #'elgrep-search ,dir ,file-name-re (quote ,re) '(,@options))
			   ,dir ,file-name-re (quote ,re) '(,@options))
		  (let ((buf ,(current-buffer)))
		    (when (buffer-live-p buf)
		      (message "Finishing elgrep thread.")
		      (with-current-buffer buf
			(setq elgrep-thread nil))
		      (when (derived-mode-p 'elgrep-menu-mode)
			(elgrep-reset-start-button)))))))))
     (async
      (let ((elgrep-path (locate-library "elgrep")))
	(async-start
	 `(lambda ()
	    (package-initialize)
	    (setq elgrep-data-file nil)
	    (load-library ,elgrep-path)
	    (cons
	     (apply #'elgrep-search ,dir ,file-name-re (quote ,re) '(,@options))
	     (and (buffer-live-p elgrep-log-buffer)
		  (with-current-buffer
		      elgrep-log-buffer
		    (buffer-string)))))
	 `(lambda (filematches-and-log)
	    (apply #'elgrep-show (car filematches-and-log) ,dir ,file-name-re (quote ,re) '(,@options))
	    (elgrep-reset-start-button ,(plist-get options :elgrep-menu))
	    (when (stringp (cdr filematches-and-log))
	      (elgrep-log "%s" (cdr filematches-and-log)))))))
     (t
      (apply #'elgrep-show (apply #'elgrep-search dir file-name-re re options)
	    dir file-name-re re options)))))

(defun elgrep-required-matches (fun req)
  "Return t when we find each required match from REQ by FUN.
Thereby REQ is a list of matchers.
FUN is a function with the same args as `re-search-forward'.
Each matcher is a predicate function or a regexp.

Moves point to `point-min'
to prepare the buffer for the actual search
when all requirements are fulfilled."
  (catch :failed
    (dolist (re req)
      (goto-char (point-min))
      (cond
       ((functionp re)
	(unless (funcall re)
	  (throw :failed nil)))
       ((stringp re)
	(let* ((neg (if	(eq (string-to-char re) elgrep-negating-char)
			(progn
			  (setq re (substring re 1))
			  t)
		      (when (string-match (string ?\\ elgrep-negating-char) re)
			(setq re (substring re 1)))
		      nil))
	       (matching (and (funcall fun re nil 'noError) t)))
	  (when (equal neg matching)
	    (throw :failed nil))
	  ))
       (t
	(error "Entry of required matches is neither a function nor a string"))
       ))
    (goto-char (point-min))
    t))

(defmacro elgrep-with-wide-buffer (&rest body)
  "Save restriction, widen buffer, and eval BODY."
  (declare (debug body))
  `(save-restriction
     (widen)
     ,@body))

(defun elgrep-occur-search (re &rest options)
  "Collect lines matching RE in the records of the current buffer.

The following set of OPTIONS is described in the help of `elgrep':
:c-op
:c-beg
:c-beg-only
:c-end
:c-end-only
:case-fold-search
:r-beg
:r-end
:search-fun

The return value is a list of matches.
Each match is a cons `(,(current-buffer) . MATCHDATA).
The structure of MATCHDATA is described in the doc string of `elgrep-search'."
  (let* ((c-op (or (plist-get options :c-op) 'buffer-substring-no-properties))
	 (c-beg (or (plist-get options :c-beg) 0))
	 (c-beg-only (plist-get options :c-beg-only))
	 (c-end (or (plist-get options :c-end) 0))
	 (c-end-only (plist-get options :c-end-only))
	 (case-fold-search (plist-get options :case-fold-search))
	 (r-beg (or (plist-get options :r-beg) #'point-min))
	 (r-end (or (plist-get options :r-end) #'point-max))
	 (search-fun (or (plist-get options :search-fun) #'re-search-forward))
	 matches
	 (last-pos (point-min))
	 (last-line-number 1))
    (when (and (consp r-beg)
	       (eq (car r-beg) t))
      (setq r-beg (cdr r-beg)
	    r-end t))
    (elgrep-with-records r-beg r-end
      (let (match
	    (required-matches (cdr-safe re))
	    (re-str (or (car-safe re)
			re))
	    (point-prev 0)
	    pos-found)
	(when (elgrep-required-matches search-fun required-matches)
	  (while (or (and
		      (setq pos-found (funcall search-fun re-str nil 'noError))
		      (or (< point-prev (setq point-prev (point)))
			  (progn
			    (setq pos-found nil)
			    (and (null (eobp))
				 (goto-char (1+ point-prev))))))
		     (null (or (eq point-prev (setq point-prev (point)))
			       (eobp))))
	    (thread-yield)
	    (when-let* (pos-found
			(n (/ (length (match-data)) 2))
			(context-beginning
			 (elgrep-line-position
			  c-beg
			  (match-beginning 0)
			  (match-beginning 0)
			  c-beg-only
			  line-beginning-position
			  re-search-backward))
			(context-end
			 (elgrep-line-position
			  c-end
			  (match-end 0)
			  context-beginning
			  c-end-only
			  line-end-position
			  re-search-forward))
			(matchdata (and
				    (<= (match-end 0) context-end)
				    (cl-loop
				     for i from 0 below n
				     collect
				     (list :match (match-string-no-properties i)
					   :context (funcall c-op context-beginning context-end)
					   :line (elgrep-with-wide-buffer
						  (setq last-line-number
							(+ last-line-number
							   (count-lines last-pos (line-beginning-position))))
						  (setq last-pos (line-beginning-position))
						  last-line-number)
					   :context-beg context-beginning
					   :context-end context-end
					   :beg (match-beginning i)
					   :end (match-end i))))))
	      (setq match (cons matchdata match)))))
	(when match
	  (setq matches (cons (cons (current-buffer) (nreverse match)) matches)))))
    (nreverse matches)))

(defcustom elgrep-command-args-short t
  "Don't ask for options in interactive calls of `elgrep' and `elgrep-occur'."
  :type 'boolean
  :group 'elgrep)

(defun elgrep-read-args (&optional files)
  "Read arguments for commands `elgrep' and `elgrep-occur'.
If FILES is non-nil also ask for the directory and the file regexp.
With non-nil SHORT, the short form is used just asking
for the directory, the file regexp, and the text search regexp.
In the short form the prefix arg is translated into
setting the option :recursive to t."
  (append
   (when files
     (let ((dir (read-directory-name "Directory: ")))
       (list dir
	     (let ((default-file-name-regexp (elgrep-default-filename-regexp dir)))
	       (if elgrep-command-args-short
		   (read-regexp (concat "File-name regexp (defaults:\"\" and \"" default-file-name-regexp "\"): "))
		 (elgrep-read-regexp-or-function
		  "File-name filter"
		  (list "" default-file-name-regexp) 'elgrep-file-name-re-hist)
		 )))))
   (list (read-regexp "Emacs regexp: " nil 'elgrep-re-hist))
   (if elgrep-command-args-short
       (list :recursive current-prefix-arg
	     :interactive t ;; during debugging `called-interactively-p' returns nil
	     )
     (cl-loop
      for opt = (completing-read "Option (try TAB for completion; empty input ends the list): "
				 (mapcar #'car elgrep-menu-arg-alist)
				 (lambda (opt)
				   (null (eq (car (alist-get opt elgrep-menu-arg-alist)) 'ignore)))
				 t)
      until (string-empty-p opt)
      collect (intern (concat ":" opt))
      collect (let ((expr (nth 1 (alist-get (intern opt) elgrep-menu-arg-alist))))
		(eval expr))
      ))))

;;;###autoload
(defun elgrep-occur (regexp &rest options)
  "Run elgrep `occur'-like for REGEXP on the current buffer.
OPTIONS are the same as for the command `elgrep'."
  (interactive (elgrep-read-args))
  (save-excursion
    (apply #'elgrep-show
     (apply #'elgrep-occur-search regexp options)
     default-directory
     nil regexp
     :interactive t
     options)))

;;;###autoload
(defun elgrep-search (dir file-name-re re &rest options)
  "In path DIR grep files with name matching FILE-NAME-RE for text matching RE.
This is done via Emacs Lisp (no dependence on external grep).
Return list of filematches.

RE may be a list of regular expressions.
In that case each file is searched for all occurences
of the first regular expression if each of the other
regular expressions occur at least once in the file.

Each filematch is a cons (file . matchdata).
file is the file name.
matchdata is a list of matches.
Each match is a list of sub-matches.
Each submatch is a plist of :match, :context, :line,
:linestart, :beg, :end, :context-beg and :context-end.

FILE-NAME-RE can also be a list of FILE-NAME-MATCHERs.

Each FILE-NAME-MATCHER can be a regular expression
or a list of a regular expression and substitution strings
\(RE FILTER1 FILTER2 ...)
The FILTERs are treated as regular expressions but the references
like \\1 are inserted in a quoted form (via `regexp-quote').

If SUBST1 starts with ?\\! then file names matching SUBST1
without the leading ?\\! will be filtered out.

Example: (\"\\\\(.*\\\\)\\.k\\\\'\" \"!\\\\1\\.\\\\(h\\\\|cpp\\\\)\\\\'\")

See `elgrep' for the valid options in plist OPTIONS."
  (setq dir (elgrep-dir-name dir))
  (with-current-buffer (get-buffer-create (or
					   (let ((buf (plist-get options :search-buffer)))
					     (and (buffer-live-p buf)
						  buf))
					   " *elgrep-search*"))
    (buffer-disable-undo)
    (setq default-directory dir)
    (unless (plist-get options :depth)
      (setq options (plist-put options :depth 0)))
    (when (or
	   (null (plist-member options :case-fold-search))
	   (eq (plist-get options :case-fold-search) 'default))
      (setq options (plist-put options :case-fold-search case-fold-search)))
    (let ((files (elgrep-directory-files dir (plist-get options :abs) file-name-re))
	  filematches
	  (depth (plist-get options :depth))
	  (mindepth (or (plist-get options :mindepth) 0))
	  (maxdepth (or (plist-get options :maxdepth) most-positive-fixnum))
	  (exclude-file-re (plist-get options :exclude-file-re))
	  (symlink (plist-get options :symlink))
	  (file-fun (plist-get options :file-fun)))
      (cond
       ((or (null exclude-file-re)
	    (and (stringp exclude-file-re) (string-equal exclude-file-re ""))))
       ((stringp exclude-file-re)
	(setq files (cl-remove-if (lambda (fname) (string-match exclude-file-re fname)) files)))
       ((functionp exclude-file-re)
	(setq files (funcall exclude-file-re files)))
       (t
	(error "Expect regexp or filter function as value for option :exclude-file-re")))
      (unless symlink
	(setq files (cl-remove-if #'file-symlink-p files)))
      (when (functionp file-fun)
	(setq files (cl-remove-if-not file-fun files)))
      (cl-loop
       for file in files do
       (when (and (file-regular-p file)
		  (or (file-readable-p file)
		      (progn
			(elgrep-log "File %S not readable." file)
			nil))
		  (>= depth mindepth))
	 (thread-yield)
	 (if re
	     (elgrep-prepare-buffer file dir options
	       (setq filematches
		     (append filematches
			     (mapcar
			      (lambda (match)
				(setcar match file)
				match)
			      (apply #'elgrep-occur-search re options)))))
	   ;; no re given; just register file with dummy matchdata
	   (setq filematches (cons (list file) filematches)))))
      (when (and (plist-get options :recursive)
		 (< depth maxdepth))
	(setq files (cl-loop
		     with path
		     for file in (elgrep-directory-files dir)
		     if (and
			 (file-directory-p (setq path (expand-file-name file dir)))
			 (or (file-accessible-directory-p path)
			     (progn (thread-yield)
				    (elgrep-log "Directory %S not accessible\n")
				    nil))
			 (or symlink (null (file-symlink-p path)))
			 (let ((dir-re (plist-get options :dir-re))
			       (exclude-dir-re (plist-get options :exclude-dir-re)))
			   (and (or (null dir-re)
				    (string-match dir-re file))
				(null
				 (and exclude-dir-re
				      (null (string-equal exclude-dir-re ""))
				      (string-match exclude-dir-re file)))))
			 (null (string-match "^\\.[.]?\\'" file)))
		     collect file))
	(let ((deep-options (plist-put (cl-copy-list options) :depth (1+ depth))))
	  (dolist (file files)
	    (thread-yield)
	    (setq filematches
		  (append
		   (if (plist-get options :abs)
		       (apply #'elgrep-search (expand-file-name file dir) file-name-re re :keep-elgrep-buffer t deep-options)
		     (let ((files (apply #'elgrep-search (expand-file-name file dir) file-name-re re :keep-elgrep-buffer t deep-options)))
		       ;;(debug)
		       (cl-loop for f in files do
				(setcar f (file-relative-name (expand-file-name (car f) file))))
		       files))
		   filematches)))))
      filematches)))

(defun elgrep-split-args-options (args)
  "Separate elgrep arguments from options in ARGS.
return a list ((DIR FILE-NAME-RE RE) OPTIONS)."
  (apply (lambda (dir file-name-re re &rest options)
	   (list (list dir file-name-re re) options))
	 args))
;; Test:
;; (elgrep-split-args-options '("dir" "file-name-re" "re" :interactive t :buffer "*elgrep*"))

(defun elgrep-rerun (&optional edit)
  "Rerun `elgrep' in Elgrep buffer with previous arguments.
Give the user the possibility to edit the command if EDIT is non-nil."
  (interactive "P")
  (unless (derived-mode-p 'elgrep-mode)
    (error "`elgrep-rerun' can only be called in elgrep buffers"))
  (cl-multiple-value-bind (args options) (elgrep-split-args-options elgrep-args)
    (setq options (plist-put options :buffer (buffer-name))
	  args (append args options))
    (if edit
	(edit-and-eval-command
	 "Elgrep-command:"
	 (cons 'elgrep/i args))
      (apply #'elgrep args))))

(defun elgrep-show (filematches dir file-name-re re &rest options)
  "Show FILEMATCHES generated by `elgrep-search'.
The parameters DIR FILE-NAME-RE RE OPTIONS are the same as for `elgrep-search'.
See `elgrep' for the valid options in the plist OPTIONS."
  (when (or (plist-get options :interactive) (called-interactively-p 'any))
    (unless dir
      (setq dir (or default-directory)))
    (let ((inhibit-read-only t))
      (with-current-buffer (get-buffer-create (or (plist-get options :buffer) "*elgrep*"))
	(if filematches
	    (progn
	      (unless (plist-get options :abs)
		(setq default-directory dir))
	      (erase-buffer)
	      (if re
		  (progn
		    (elgrep-list-matches filematches options)
		    (elgrep-mode)
		    (goto-char (point-min)))
		(elgrep-dired-files (mapcar 'car filematches)))
	      (setq elgrep-args (append
				 (list dir file-name-re re)
				 options))
	      (set-buffer-modified-p nil)
	      (display-buffer (current-buffer)))
	  (unless (plist-get options :keep-elgrep-buffer)
	    (kill-buffer))
	  (message "elgrep: No matches for \"%s\" in files \"%s\" of dir \"%s\"." re file-name-re dir)))))
  filematches)

;;;###autoload
(require 'easymenu)
;;;###autoload
(easy-menu-add-item global-map '("menu-bar" "tools") ["Search Files (Elgrep)..." elgrep-menu t] "grep")

(defvar next-error-highlight-no-select) ;; defined in "simple.el"

(defun elgrep-first-error-no-select (&optional n)
  "Restart at first error.
Visit corresponding source code.
With prefix arg N, visit the source code of the Nth error."
  (interactive "p")
  (let ((next-error-highlight next-error-highlight-no-select))
    (next-error n t))
  (pop-to-buffer next-error-last-buffer))

(defun elgrep-in-read-only-p (&optional pos)
  "Check whether characters before and after POS are read-only."
  (unless pos (setq pos (point)))
  (save-restriction
    (widen)
    (and (> pos (point-min))
	 (get-text-property (1- pos) 'read-only)
	 (get-text-property pos 'read-only))))

(defvar rectangle-mark-mode)

(defun elgrep-region-extract-ad (fun method)
  "Apply FUN with METHOD when extracting region in `elgrep-edit-mode'.
Intended as buffer-local `add-function' for `region-extract-function'.
Don't allow deletion for:
- rectangles
- region bounds within read-only text."
  (require 'rect)
  (cond
   ((or (memq method '(nil bounds))
	(null
	 (or
	  rectangle-mark-mode
	  (elgrep-in-read-only-p (region-beginning))
	  (elgrep-in-read-only-p (region-end)))))
    (let ((inhibit-read-only t))
      (funcall fun method)))
   (rectangle-mark-mode
    (user-error
     "Deleting rectangles not supported yet in `elgrep-mode'"))
   (t
    (signal 'text-read-only nil))))

(defmacro elgrep-edit-advice (fun)
  "Define elgrep-FUN-function and elgrep-FUN-ad for FUN.
Advice FUN with elgrep-FUN-ad such that it calls
the function registered at elgrep-FUN-function if that variable is non-nil."
  (let ((elgrep-fun-function (intern (format "elgrep-%s-function" fun)))
	(elgrep-fun-ad (intern (format "elgrep-%s-ad" fun))))
    `(progn
       (defvar-local ,elgrep-fun-function (symbol-function (quote ,fun))
	 ,(format "Called to do the work of `%s' if non-nil." fun))

       (defun ,elgrep-fun-ad (fun &rest args)
	 ,(format "Call FUN with ARGS if `elgrep-%s-function' is nil.
If `elgrep-%s-function' is non-nil
call that function with ARGS instead." fun fun)
	 (if ,elgrep-fun-function
	     (apply ,elgrep-fun-function args)
	   (apply fun args)))

       (advice-add (quote ,fun) :around (function ,elgrep-fun-ad)))))

(elgrep-edit-advice flush-lines)
(elgrep-edit-advice keep-lines)

(defun elgrep-inhibit-read-only-ad (fun &rest args)
  "Call FUN with ARGS and `inhibit-read-only' set to t."
  (let ((inhibit-read-only t))
    (apply fun args)))

(defun elgrep-delete-region (b e)
  "Delete region from B to E like `delete-region'.
Abort if B or E is in the middle of a read-only region."
  (interactive "r")
  (when (or (elgrep-in-read-only-p b)
	    (elgrep-in-read-only-p e))
    (signal 'text-read-only nil))
  (let ((inhibit-read-only t))
    (delete-region b e)))

(defun elgrep-compilation-message-at-point (&optional pt)
  "Return compilation message corresponding to PT."
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (or pt (setq pt (point)))
  (let ((msg (or (get-text-property pt 'compilation-message)
		 (and
		  (setq pt (previous-single-property-change pt 'compilation-message))
		  (get-text-property (1- pt) 'compilation-message)))))
    msg))

(defun elgrep-beginning-of-compilation-message (&optional pt)
  "Return position of compilation message at PT.
Return nil if PT is a position before the first compilation message."
  (interactive)
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (or pt (setq pt (point)))
  (save-restriction
    (widen)
    (compilation--ensure-parse pt)
    (if (get-text-property pt 'compilation-message)
	(if (or (eq pt (point-min))
		(null (get-text-property (1- pt) 'compilation-message)))
	    pt
	  (previous-single-property-change pt 'compilation-message))
      (and
       (setq pt (or
		 (and (get-text-property (1- pt) 'compilation-message) (point))
		 (previous-single-property-change pt 'compilation-message)))
       (or (previous-single-property-change pt 'compilation-message) (point-min))))))

(defun elgrep-end-of-compilation-message (&optional pt)
  "Return end of compilation message at PT.
Returns nil if PT is a potision before the first compilation message."
  (interactive)
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (or pt (setq pt (point)))
  (save-restriction
    (widen)
    (compilation--ensure-parse pt)
    (when (setq pt (elgrep-beginning-of-compilation-message pt))
      (setq pt (next-single-property-change pt 'compilation-message))
      (or (next-single-property-change pt 'compilation-message)
	  (point-max)))))

(defun elgrep-kill-compilation-message (&optional pt)
  "Kill compilation message at PT."
  (interactive)
  (unless pt (setq pt (point)))
  (let ((b (elgrep-beginning-of-compilation-message pt))
	(e (elgrep-end-of-compilation-message pt))
	(inhibit-read-only t))
    (when (and b e)
      (kill-region b e))))

(define-key elgrep-mode-map (kbd "C-c C-k") #'elgrep-kill-compilation-message)
(define-key elgrep-mode-map (kbd "C-c C--") #'elgrep-kill-compilation-message)

(defun elgrep-parse-compilation-message (&optional end rules)
  "Parse next compilation message up to END.
Use RULES or `compilation-error-regexp-alist' for parsing.
Return an alist with keys: file, line, col, type, link.
Each key maps to a list of the matching string, the beginning of the match
and the end of the match."
  ;; Partially stolen from `compilation-parse-errors':
  (setq rules (or rules compilation-error-regexp-alist))
  (let (ret)
    (while rules
      (let ((item (car rules))
	    (number-entry (lambda (subexpr)
			    (if (numberp subexpr)
				(list
				 (string-to-number (match-string subexpr))
				 (match-beginning subexpr)
				 (match-end subexpr))
			      (list (funcall subexpr))))))
	(if (symbolp item)
            (setq item (cdr (assq item
				  compilation-error-regexp-alist-alist))))
	(let ((file (nth 1 item))
              (line (nth 2 item))
              (col (nth 3 item))
              (type (nth 4 item))
	      (link (nth 5 item))
              (pat (car item)))
	  (cond
	   ((not (memq 'omake compilation-error-regexp-alist)) nil)
	   ((string-match "\\`\\([^^]\\|\\^\\( \\*\\|\\[\\)\\)" pat)
            nil) ;; Not anchored or anchored but already allows empty spaces.
	   (t (setq pat (concat "^\\(?:      \\)?" (substring pat 1)))))
	  (if (consp file)	(setq file (car file)))
	  (if (consp line)	(setq line (car line)))
	  (if (consp col)	(setq col (car col)))
	  (unless (or (null link) (integerp link))
            (error "HYPERLINK should be an integer: %s" link))
	  (if (re-search-forward pat end t)
	      (setq rules nil
		    ret
		    `(
		      (file ,(match-string file) ,(match-beginning file) ,(match-end file))
		      (line . ,(funcall number-entry line))
		      (col . ,(funcall number-entry col))
		      (type ,type)
		      (link . ,(and link
				    (list
				     (match-string link)
				     (match-beginning link)
				     (match-end link)))))))
	  (setq rules (cdr rules)))))
    ret))

(defun elgrep-compilation-message-modify (expr)
  "Apply EXPR to all compilation messages following point.
Thereby, the symbols 'file, 'line, 'col, 'type, 'link
are locally let-bound corresponding to the current
compilation message.
The values are those returned by `elgrep-parse-compilation-message'."
  (interactive "xLisp expression with local file, line, col, type, link:")
  (save-excursion
    (let (msg
	  (inhibit-read-only t))
      (while (setq msg (elgrep-parse-compilation-message))
	(eval expr msg)))))

(defun elgrep-save-collect ()
  "Collect all entries from the current elgrep result buffer.
Return an alist mapping the files to modification lists.
Each modification in the modification list is a list (BEG END CONTEXT EDITED).
BEG and END are the beginning and the end of the context.
CONTEXT is what `elgrep-search' found in the file.
EDITED is the edited text in the elgrep buffer."
  (cl-assert (eq major-mode 'elgrep-mode) nil
	     "Major mode of buffer %s is not `elgrep-mode'" (current-buffer))
  (let (file-mod-alist)
    (goto-char (point-min))
    (while (and
	    (null (eobp))
	    (condition-case nil
		(progn (compilation-next-error 1) ;; barfs if the buffer does not contain any message at all
		       t)
	      (error nil)))
      (let* ((beg (point))
	     (context (get-text-property (point) 'elgrep-context))
	     (edited-string (buffer-substring-no-properties (goto-char (next-single-property-change (point) 'compilation-message)) (goto-char (1- (or (next-single-property-change (point) 'compilation-message)
																		      (point-max)))))))
	(when (and (stringp context)
		   (null (string-equal edited-string context)))
	  (let* ((msg (get-text-property beg 'compilation-message))
		 (loc (compilation--message->loc msg))
		 (file-struct (compilation--loc->file-struct loc))
		 (name (caar file-struct))
		 (dir (cadar file-struct))
		 (path (if dir (expand-file-name name (file-name-directory dir)) name))
		 (entry (or
			 ;; first try to get a direct match because this is faster
			 ;; (it may be that there are many file-mod-alist in one file)
			 (assoc-string path file-mod-alist)
			 (cl-assoc path file-mod-alist :test #'file-equal-p)
			 (car (setq file-mod-alist (cons (list path) file-mod-alist))))))
	    (setcdr entry
		    (cons
		     (list
		      (get-text-property beg 'elgrep-context-begin)
		      (get-text-property beg 'elgrep-context-end)
		      context
		      edited-string)
		     (cdr entry)))))))
    (mapc
     (lambda (file-modifications)
       ;; Don't touch (car file-modifications). It is the file name.
       (setcdr file-modifications
	       (cl-sort (cdr file-modifications) #'> :key #'car)))
     file-mod-alist)
    file-mod-alist))

(defun elgrep-save (&optional _really-save)
  "Apply modifications in the current elgrep buffer to the files.
The argument _REALLY-SAVE is for compatibility only."
  (interactive)
  (let ((file-matches-alist (elgrep-save-collect))
	(file-fun (plist-get (elgrep-args-options elgrep-args) :file-fun)))
    (save-excursion
      (dolist (file-matches file-matches-alist)
	(let ((file (car file-matches))
	      (matches (cdr file-matches))
	      file-modified)
	  (if (file-writable-p file)
	      (with-temp-buffer
		(insert-file-contents file)
		(dolist (match matches)
		  (cl-multiple-value-bind
		      (context-begin context-end context edited-str)
		      match
		    (goto-char context-begin)
		    (let* ((original-str (buffer-substring-no-properties context-begin context-end)))
		      (when (and (string-equal context original-str) ;; It is still the old context...
				 (null (string-equal edited-str original-str))) ;; and this has been changed in *elgrep*.
			(setq file-modified t)
			(kill-region context-begin context-end)
			(insert edited-str)))))
		(when file-modified
		  (let ((new-name (or (and (functionp file-fun)
					   (funcall file-fun file))
				      file)))
		    (write-file (or (and (stringp new-name) new-name)
				    file))))))))))
  (set-buffer-modified-p nil))

(defvar elgrep-edit-mode-map (let ((map (copy-keymap global-map)))
                               (define-key map [remap save-buffer] #'elgrep-save)
			       (define-key map (kbd "C-c C-n") #'next-error-no-select)
			       (define-key map (kbd "C-c C-p") #'previous-error-no-select)
			       (define-key map (kbd "C-c C-f") #'elgrep-first-error-no-select)
                               map)
  "Keymap used in function `elgrep-edit-mode'.
Ovwerrides `compilation-mode-map'.")
(defvar-local elgrep-saved-major-mode nil)

(defun elgrep-enrich-text-property (refprop prop-list)
  "Enrich intervals with text property REFPROP.
Use the list of text properties PROP-LIST."
  (let (interval) ;; This should not be necessary!
    (cl-loop for interval being the intervals property refprop
	     when (get-text-property (car interval) refprop)
	     do (add-text-properties (car interval) (cdr interval) prop-list))))

(defvar-local elgrep-edit-previous-header nil
  "Elgrep-edit-mode is a minor mode that can be switched on and off.
When it is switched off it should restore
the old header line which is preserved here.")

(define-minor-mode elgrep-edit-mode
  "Mode for editing compilation buffers (especially elgrep buffers)."
  :init-value nil
  :lighter " e"
  :keymap nil
  '(([remap delete-region] . elgrep-delete-region))
  (cl-assert (derived-mode-p 'elgrep-mode) nil "Major mode not derived from compilation mode.")
  (if elgrep-edit-mode
      (progn
	(unless elgrep-edit-previous-header ; Protect against re-entry of function `elgrep-edit-mode' with non-nil `elgrep-edit-mode'.
	  (setq elgrep-edit-previous-header header-line-format))
	(setq header-line-format (substitute-command-keys "Exit elgrep-edit-mode: \\[elgrep-edit-mode]; Save modifications: \\[elgrep-save]"))
        (setq buffer-read-only nil)
	(add-function :around
		      (local 'region-extract-function)
		      #'elgrep-region-extract-ad)
	(mapc (lambda (var)
		(add-function :around
			      (local var)
			      #'elgrep-inhibit-read-only-ad))
	      '(elgrep-flush-lines-function
		elgrep-keep-lines-function))
        (define-key (current-local-map) [remap self-insert-command] nil)
	(with-silent-modifications
	  (elgrep-enrich-text-property 'compilation-message '(read-only t intangible t)))
        (when (eq buffer-undo-list t)
          (setq buffer-undo-list nil)
          (set-buffer-modified-p nil)))
    (setq header-line-format elgrep-edit-previous-header
	  elgrep-edit-previous-header nil
	  buffer-read-only t)
    (remove-function (local 'region-extract-function)
		     #'elgrep-region-extract-ad)
    (mapc (lambda (var)
	    (remove-function (local var)
			     #'elgrep-inhibit-read-only-ad))
	  '(elgrep-flush-lines-function
	    elgrep-keep-lines-function))
    (define-key (current-local-map) [remap self-insert-command] 'undefined)))

(defalias 'elgrep-edit #'elgrep-edit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling the data file:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun elgrep-load-elgrep-data-file ()
  "Load the `elgrep-data-file'."
  (interactive)
  (when (stringp elgrep-data-file)
    (let ((file (expand-file-name elgrep-data-file user-emacs-directory)))
      (load file t nil t))))

(elgrep-load-elgrep-data-file)

;;;###autoload
(defun elgrep-save-elgrep-data-file ()
  "Save the elgrep data file if `elgrep-data-file' is a string.
This can be used as `kill-emacs-hook'.
Unconditionally return the value of `elgrep-data-file'."
  (interactive)
  (when (stringp elgrep-data-file)
    (with-temp-buffer
      (let (print-level print-length)
	(insert (format "%S" `(setq elgrep-call-list (quote ,elgrep-call-list)))))
      (write-file
       (expand-file-name elgrep-data-file user-emacs-directory))))
  elgrep-data-file)

(add-hook 'kill-emacs-hook #'elgrep-save-elgrep-data-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers for defining search expressions

(defun elgrep/point-min ()
  "Move point to `point-min' and return point."
  (goto-char (point-min)))

(defun elgrep/point-max ()
  "Move point to `point-max' and return point."
  (goto-char (point-max)))

(defun elgrep/forward-sexp ()
  "Move point forward one sexp and return point."
  (forward-sexp)
  (point))

(defun elgrep/up-list ()
  "Move point out of current list and return point."
  (up-list)
  (point))

(defun elgrep/process-options (option-defaults body)
  "Process plist of OPTION-DEFAULTS occuring at start of BODY.
The keys in OPTION-DEFAULTS are also the potential option keys in BODY.
The values in OPTION-DEFAULTS are the default values for the options.
The options are removed from BODY
and the list (OPTIONS BODY) of actual OPTIONS and modfied BODY is returned.
The recommended way for processing options is:
\(cl-destructuring-bind (OPTIONS BODY) (filesTZA-process-options OPTIONS-DEFAULTS BODY) ...)."
  (setq option-defaults (cl-copy-list option-defaults))
  (while
      (cl-loop for opt on option-defaults by #'cddr
	       when (eq (car body) (car opt))
	       do (setf (cadr opt) (cadr body)
			body (cddr body))
	       and return t
	       finally return nil))
  (list option-defaults body))

(defmacro elgrep/with-current-file (filename &rest body)
  "Temporarily visit FILENAME to execute BODY.
If a buffer is already visiting FILENAME re-use that buffer.
Otherwise create a new buffer for visiting FILENAME
and kill that buffer if it is unmodified after executing BODY.
BODY can start with option-value pairs where the valid options are
:nowarn and :rawfile with the meaning described in the help for
of `find-file-noselect'."
  (declare (indent 1) (debug (form body)))
  (let ((file-buffer (make-symbol "file-buffer"))
	(file-name (make-symbol "file-name"))
	(old-file-buffer (make-symbol "old-file-buffer")))
    (cl-destructuring-bind (options body) (elgrep/process-options '(:nowarn nil :rawfile nil) body)
      `(let* ((,file-name ,filename)
	      (,old-file-buffer (find-buffer-visiting ,file-name))
	      (,file-buffer (or ,old-file-buffer
                                (find-file-noselect ,file-name ,(plist-get options :nowarn) ,(plist-get options :rawfile)))))
	 (with-current-buffer ,file-buffer
           (unwind-protect
	       (progn
		 ,@body)
	     (unless (or ,old-file-buffer
			 (buffer-modified-p))
	       (kill-buffer))))))))

(defun elgrep/narrow (begin end &optional begin-dir end-dir)
  "Narrow to region from BEGIN to END.
Put point to begin.
BEGIN and END can be buffer positions, functions, or regexps.

If one bound BEGIN or END is a function apply that function
without arguments.
The corresponding bound is point after the function call
if the function returns non-nil or moves point.

If one bound BEGIN or END is a regexp search for that bound.
The bound position is at the beginning of the match.

BEGIN-DIR and END-DIR indicate the start of the search and the search direction
They can be one of the following characters:
s: forward from start of accessible buffer
f: forward from point
b: backward from point
e: backward from end of accessible buffer

The default for BEGIN-DIR and END-DIR is f.
The direction options s and e do also move point if BEGIN or END is a function.

After the search for BEGIN point is at the beginning of the region.
The search for END is always limited to the region from BEGIN to
end of buffer.  Thus the characters s and f do the same for END.
Return non-nil if the range from BEGIN to END is valid."
  (interactive
   (list
    (read-regexp "Regexp for begin")
    (read-regexp "Regexp for end")
    (read-multiple-choice "Start searching for begin " '((?s "forward from start of accessible buffer") (?f "forward from point") (?b "backward from point") (?e "backward from end of accessible buffer")))
    (read-multiple-choice "Search backwards for end? " '((?s "forward from start of region") (?e "backward from end of accessible buffer")))))
  (let* ((pt (point))
	 (search (lambda (target dir what)
		   (cond
		    ((functionp target)
		     (cl-case dir
		       (?s (goto-char (point-min)))
		       (?e (goto-char (point-max))))
		     (let ((pt1 (point)))
		       (and (or (funcall target)
				(null (eq pt1 (point))))
			    (point))))
		    ((number-or-marker-p target)
		     (when (<= target 0)
		       (setq target (+ (point-max) target)))
		     (when (and
			    (> target (point))
			    (<= target (point-max)))
		       (goto-char target)))
		    ((stringp target)
		     (if (memq dir '(?b ?e))
			 (let (start)
			   (when (eq dir ?e)
			     (setq start (point))
			     (goto-char (point-max)))
			   (re-search-backward target start t))
		       (when (re-search-forward target nil t)
			 (goto-char (match-beginning 0)))))
		    (t (user-error "Invalid %s for elgrep-narrow" what)))))
	 (begin-pos (progn
		      (when (eq begin-dir ?s)
			(goto-char (point-min)))
		      (funcall search begin begin-dir "begin")))
	 (end-pos (funcall search end end-dir "end")))
    (if (and begin-pos end-pos)
	(progn
	  (narrow-to-region begin-pos end-pos)
	  (goto-char begin-pos))
      (goto-char pt)
      nil)))
;; test:
;; (elgrep/narrow "^(defun elgrep/narrow" "^;; test" ?s)
;; (elgrep/narrow "^(defun elgrep/narrow" "^invalid search" ?s)
;; (elgrep/narrow (progn (goto-char (point-min)) (re-search-forward "^(defun elgrep/narrow")) -1)
;; (elgrep/narrow "^(defun elgrep/narrow" "^;; test" ?b)
;; (progn (search-backward "elgrep/narrow") (elgrep/narrow 'backward-up-list 'forward-sexp))

(defun elgrep/keyval (key &optional separator end goto)
  "Search for lines beginning with any match of KEY SEPARATOR.
Ignore spaces before and after KEY and after SEPARATOR.
Search starts at point.
VALUE is read as the following sexp.
If END is a string sexps behind SEPARATOR are read until
END or the end of the accessible part of the buffer is reached.

SEPARATOR defaults to \"\\\\([:=]\\\\|:=\\\\)?\".

If SEPARATOR is a function instead of a regexp
call that function with KEY and END as arguments.
It should return non-nil if KEY is found and move point
at the beginning of the value maybe with leading whitespace.

Options for GOTO:
nil or unset: do not move point
key: goto end of key
sep: goto end of separator
val: goto end of value

Return the value as string if KEY is found
and return nil otherwise."
  (unless separator
    (setq separator "\\([:=]\\|:=\\|[[:space:]]\\)"))
  (let ((pt (point))
	value)
    (when (if (functionp separator)
	      (funcall separator key end)
	    (re-search-forward (concat "^[[:space:]]*\\(" key "\\)[[:space:]]*" separator) nil t))
      (cl-case goto
	(key (setq pt (match-end 1)))
	(sep (setq pt (point))))
      (parse-partial-sexp (point) (point-max) nil t (syntax-ppss))
      (let ((b (point)))
	(if (stringp end)
	    (while
		(and (null (eobp))
		     (null (looking-at end)))
	      (forward-sexp))
	  (forward-sexp))
	(when (eq goto 'val)
	  (setq pt (point)))
	(setq value (buffer-substring-no-properties b (point))))
      (goto-char pt))
    value))
;; test:
;; (save-excursion (goto-char (point-min)) (elgrep/keyval "(defun[[:space:]]elgrep/keyval"))
;; (save-excursion (goto-char (point-min)) (elgrep/keyval "(defun[[:space:]]elgrep/keyval" nil ")")) ;; testing end
;; (progn (goto-char (point-min)) (elgrep/keyval "(defun[[:space:]]elgrep/keyval" nil ")" 'val)) ;; testing goto
;; (progn (goto-char (point-min)) (elgrep/keyval "(defun[[:space:]]elgrep/keyval" nil nil 'key)) ;; testing goto
;; (progn (goto-char (point-min)) (elgrep/keyval "(defun[[:space:]]elgrep/keyval" nil nil 'sep)) ;; testing goto

(defun elgrep/string-match (regexp string &optional start)
  "Run (string-match REGEXP STRING START) if STRING is really a string."
  (and (stringp string)
       (string-match regexp string start)))

(defun elgrep/match-outer-sexp ()
  "Save outer sexp as match data if it exists and put point behind it.
Return nil and do not move point if there is no outer sexp."
  (let ((pt (point)) b)
    (condition-case nil
	(progn
	  (backward-up-list nil t t)
	  (setq b (point))
	  (forward-sexp)
	  (set-match-data (list b (point) (current-buffer)))
	  (point))
      (scan-error (goto-char pt) nil))))
;; test:
;; ( (let ((md (progn (elgrep/match-outer-sexp) (match-data)))) md) )

(declare-function bibtex-skip-to-valid-entry "bibtex.el")
(declare-function bibtex-parse-entry "bibtex.el")

(defun elgrep/bibtex-key-val (key-val-list &rest _)
  "Example search function for BibTeX files.
Search for BibTeX entries matching KEY-VAL-LIST."
  (require 'bibtex)
  (when-let* ((found (bibtex-skip-to-valid-entry))
	      (entry (bibtex-parse-entry))
	      (keyvals (read key-val-list)))
    (cl-loop
     with key-val-match
     for pair in entry
     when (setq key-val-match (assoc-string (car pair) keyvals))
     do (setq keyvals (remove key-val-match keyvals))
     and
     unless (string-match (nth 1 key-val-match) (cdr pair))
     return nil
     finally
     return (if keyvals nil
	      (set-match-data (list (car found) (cdr found) (current-buffer)))
	      (goto-char (cdr found))))))

(defun elgrep/comment-p ()
  "Return non-nil if point is in a comment.
Does not move point and does not change `match-data'."
  (save-excursion
    (save-match-data
      (comment-beginning))))

(defun elgrep/outside-comment-p ()
  "Return t if point is outside any comment."
  (null (elgrep/comment-p)))

(defun elgrep/re-search-comments (&rest args)
  "Search for regexp within comments.
The ARGS are the same as for `re-search-forward'.

If a match for REGEXP is found outside comments
put point behind the match but return nil.

\(fn REGEXP &optional BOUND NOERROR COUNT)"
  (let ((ret (apply #'re-search-forward args)))
    (and (elgrep/comment-p) ret)))

(defun elgrep/re-search-outside-comments (&rest args)
  "Search for REGEXP outside comments.
The ARGS are the same as for `re-search-forward'.

If a match for REGEXP is found within a comment put point
at the end of the match but return nil.

\(fn REGEXP &optional BOUND NOERROR COUNT)"
  (let ((ret (apply #'re-search-forward args)))
    (and (elgrep/outside-comment-p) ret)))

(defun elgrep/re-search-code (&rest args)
  "Return same as `re-search-forward' for ARGS but only for occurences in code.
Otherwise return nil.
Only works with syntax tables."
  (let ((ret (apply #'re-search-forward args)))
    (and (null (nth 8 (syntax-ppss)))
	 ret)))

(defun elgrep/re-search-goto-match-beginning (&rest args)
  "Search for regexp like `re-search-forward' but goto beginning of match.
The ARGS are the same as for `re-search-forward'.

\(fn REGEXP &optional BOUND NOERROR COUNT)"
  (and (apply #'re-search-forward args) (goto-char (match-beginning 0))))

(defun elgrep/forward-sexp-at-match-end ()
  "Go to the end of the last match and forward one sexp."
  (goto-char (match-end 0))
  (forward-sexp)
  (point))

(defcustom elgrep/git-program "git"
  "Program to call for Git command."
  :type 'string
  :group 'elgrep)

(defcustom elgrep/git-default-git-options '("--no-pager")
  "List of options for Git command."
  :type '(repeat 'string)
  :group 'elgrep)

(defcustom elgrep/git-ls-tree-options '("ls-tree" "--name-only" "HEAD")
  "List of options for Git ls-tree."
  :type '(repeat 'string)
  :group 'elgrep)

(defun elgrep/filter (string-list regexp)
  "Return only those strings in STRING-LIST that match REGEXP."
  (remove nil
	  (mapcar
	   (lambda (s)
	       (and (string-match regexp s)
		    s))
	   string-list)))
;; Tests:
;; (elgrep/filter '("first" "second") "fi") ;; only one match
;; (elgrep/filter '("first" "second") "non-matching")
;; (elgrep/filter '("first" "second") "") ;; all matching

(defun elgrep/git (files &optional file-re not-git)
  "Return only those FILES which are under Git control.
FILES can also be a single file name.
In that case the result is nil if this file is not
under Git control.

If FILE-RE is non-nil return only file names matching FILE-RE.
If NOT-GIT is non-nil list only files not under git-control."
  (when (stringp files)
    (setq files (list files)))
  (when file-re
    (setq files (elgrep/filter files file-re)))
  (with-temp-buffer
    (apply
     #'call-process
     elgrep/git-program
     nil
     t
     nil
     (append
      elgrep/git-default-git-options
      elgrep/git-ls-tree-options
      (list "--")
      files)
     )
    (let ((ret
	   (split-string (buffer-substring-no-properties
			  (point-min)
			  (progn
			    (goto-char (point-max))
			    (skip-chars-backward "\n\t ")
			    (point)))
			 "\n" t)))
      (when not-git
	(setq ret (cl-set-difference files ret :test #'string-equal)))
      ret)))
;; Test: (elgrep/git '("elgrep.el" "texfrag.el" "loaddefs.el"))
;; (elgrep/git "elgrep.el")
;; (elgrep/git "some-non-existing-file")
;; (elgrep/git '("elgrep.el" "laoddefs.el") nil t)

(defun elgrep/file-filter-modified-extension (files extension filter-extension)
  "Return list FILES of file names filtered by extension.
If the extension of a file name matches regexp EXTENSION then
files with the same basename and extension matching FILTER-EXTENSION are
filtered out of FILES.

Example:

(elgrep/file-filter-modified-extension
     '(\"1.k\" \"1.cpp\" \"1.h\" \"2.cpp\")
     \"\\\\.k\"
     \"\\\\.\\\\(cpp\\\\|h\\\\)\")

returns (\"1.k\" \"2.cpp\")."
  (let
      ((re
	(concat
	 (regexp-opt
	  (delq
	   nil
	   (mapcar
	    (lambda (fn)
	      (let ((fne (file-name-extension fn)))
		(when
		    (and fne
			 (or (string-match extension fne)
			     (string-match extension (concat "." fne))))
		  (file-name-base fn))))
	    files)))
	 filter-extension)))
    (cl-remove-if
     (lambda
       (fn)
       (string-match re fn))
     files)))
;; Test: (elgrep/file-filter-modified-extension '("f1.cpp" "f1.k" "f1.h" "f2.cpp") "\\.k" "\\.\\(cpp\\|h\\)")

(defmacro elgrep/m (&rest args)
  "Call `elgrep' with unevaluated ARGS."
  `(apply #'elgrep '(,@args)))

(defmacro elgrep/i (&rest args)
  "Call `elgrep' interactively with unevaluated ARGS.
The interactive call is accomplished by appending (:interactive t) to ARGS."
  `(apply #'elgrep '(,@args :interactive t)))

(defvar elgrep/1 nil
  "Special variable that can be used in elgrep commands.")
(defvar elgrep/2 nil
  "Special variable that can be used in elgrep commands.")
(defvar elgrep/3 nil
  "Special variable that can be used in elgrep commands.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'elgrep)
;;; elgrep.el ends here
