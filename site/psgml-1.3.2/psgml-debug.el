;;;;\filename psgml-debug.el
;;;\Last edited: 2001-03-10 00:32:00 lenst
;;;\RCS $Id: psgml-debug.el,v 2.31 2005/03/02 19:43:45 lenst Exp $
;;;\author {Lennart Staflin}
;;;\maketitle

;;\begin{codeseg}
(provide 'psgml-debug)
(require 'psgml)
(require 'psgml-parse)
(require 'psgml-edit)
(require 'psgml-dtd)
(autoload 'sgml-translate-model "psgml-dtd" "" nil)
(eval-when-compile
  (require 'cl)
  (require 'elp)
  (require 'edebug))

;;;; Debugging

(define-key sgml-mode-map "\C-c," 'sgml-goto-cache)
(define-key sgml-mode-map "\C-c\C-x" 'sgml-dump-tree)
(define-key sgml-mode-map "\C-c."   'sgml-shortref-identify)

(defun sgml-this-element ()
  (interactive)
  (let ((tree (sgml-find-element-of (point))))
    (sgml-dump-rec tree)))

(defun sgml-goto-cache ()
  (interactive)
  (setq sgml-dtd-info (sgml-pstate-dtd sgml-buffer-parse-state)
	sgml-top-tree (sgml-pstate-top-tree sgml-buffer-parse-state))
  (sgml-goto-start-point (point))
  (message "%s" (sgml-dump-node sgml-current-tree)))

(defun sgml-dump-tree (arg)
  (interactive "P")
  (when arg
    (sgml-parse-to-here))
  (with-output-to-temp-buffer "*Dump*"
    (sgml-dump-rec (sgml-pstate-top-tree sgml-buffer-parse-state))))

(defun sgml-auto-dump ()
  (let ((standard-output (get-buffer-create "*Dump*"))
	(cb (current-buffer)))

    (when sgml-buffer-parse-state
      (unwind-protect
	  (progn (set-buffer standard-output)
		 (erase-buffer))
	(set-buffer cb))

      (sgml-dump-rec (sgml-pstate-top-tree sgml-buffer-parse-state))

      ))
  )

(defun sgml-start-auto-dump ()
  (interactive)
  (add-hook 'post-command-hook
	    (function sgml-auto-dump)
	    'append))

(defun sgml-comepos (epos)
  (if (sgml-strict-epos-p epos)
      (format "%s:%s"
	      (sgml-entity-name (sgml-eref-entity (sgml-epos-eref epos)))
	      (sgml-epos-pos epos))
    (format "%s" epos)))

(defun sgml-dump-node (u)
  (format
   "%s%s start:%s(%s) end:%s(%s) epos:%s/%s net:%s\n"
   (make-string (sgml-tree-level u) ?. )
   (sgml-element-gi u)
   (sgml-element-start u) (sgml-tree-stag-len u)
   (if (sgml-tree-etag-epos u) (sgml-tree-end u)) (sgml-tree-etag-len u)
   (sgml-comepos (sgml-tree-stag-epos u))
   (sgml-comepos (sgml-tree-etag-epos u))
   (sgml-tree-net-enabled u)))

(defun sgml-dump-rec (u)
  (while u
    (princ (sgml-dump-node u))
    (sgml-dump-rec (sgml-tree-content u))
    (setq u (sgml-tree-next u))))

(defun sgml-shortref-identify ()
  (interactive)
  (sgml-find-context-of (point))
  (let* ((nobol (eq (point) sgml-rs-ignore-pos))
	 (tem (sgml-deref-shortmap sgml-current-shortmap nobol)))
    (message "%s (%s)" tem nobol)))

(defun sgml-lookup-shortref-name (table map)
  (car (rassq map (cdr table))))

(defun sgml-show-current-map ()
  (interactive)
  (sgml-find-context-of (point))
  (let ((name (sgml-lookup-shortref-name
	       (sgml-dtd-shortmaps sgml-dtd-info)
	       sgml-current-shortmap)))
    (message "Current map: %s"
	     (or name "#EMPTY"))))

;;;; For edebug

;;(put 'when 'edebug-form-hook t)
;;(put 'unless 'edebug-form-hook t)
;;(put 'push 'edebug-form-hook '(form sexp))
;;(put 'setf 'edebug-form-hook '(sexp form))

(setq edebug-print-level 3
      edebug-print-length 5
      edebug-print-circle nil
)

(eval-when (load)
  (unless sgml-running-lucid
    (def-edebug-spec sgml-with-parser-syntax (&rest form))
    (def-edebug-spec sgml-with-parser-syntax-ro (&rest form))
    (def-edebug-spec sgml-skip-upto (sexp))
    (def-edebug-spec sgml-check-delim (sexp &optional sexp))
    (def-edebug-spec sgml-parse-delim (sexp &optional sexp))
    (def-edebug-spec sgml-is-delim (sexp &optional sexp sexp sexp))))

;;;; dump

(defun sgml-dump-dtd (&optional dtd)
  (interactive )
  (unless dtd
    (setq dtd (sgml-pstate-dtd sgml-buffer-parse-state)))
  (with-output-to-temp-buffer "*DTD dump*"
    (princ (format "Dependencies: %S\n"
		   (sgml-dtd-dependencies dtd)))
    (loop for et being the symbols of (sgml-dtd-eltypes dtd)
	  do (sgml-dp-element et))))

(defun sgml-dump-element (el-name)
  (interactive
   (list (completing-read "Element: "
			  (sgml-dtd-eltypes
			   (sgml-pstate-dtd sgml-buffer-parse-state))
			  nil t)))
  (with-output-to-temp-buffer "*Element dump*"
    (sgml-dp-element (sgml-lookup-eltype el-name))))

(defun sgml-dp-element (el)
  (cond
   ((sgml-eltype-defined el)
    (princ (format "Element %s %s %s%s:\n"
		   (sgml-eltype-name el)
		   (if (sgml-eltype-stag-optional el) "O" "-")
		   (if (sgml-eltype-etag-optional el) "O" "-")
		   (if (sgml-eltype-mixed el) " mixed" "")))
    (cond
     ((sgml-model-group-p (sgml-eltype-model el))
      (sgml-dp-model (sgml-eltype-model el)))
     (t
      (prin1 (sgml-eltype-model el))
      (terpri)))
    (princ (format "Exeptions: +%S -%S\n"
		   (sgml-eltype-includes el)
		   (sgml-eltype-excludes el)))
    (princ (format "Attlist: %S\n" (sgml-eltype-attlist el)))
    (princ (format "Plist: %S\n" (symbol-plist el))))
   (t
    (princ (format "Undefined element %s\n" (sgml-eltype-name el)))))
  (terpri))


(defun sgml-dp-model (model &optional indent)
  (or indent (setq indent 0))
  (let ((sgml-code-xlate (sgml-translate-model model)))
    (loop
     for i from 0
     for x in sgml-code-xlate do
     (cond ((sgml-normal-state-p (car x))
	    (princ (format "%s%d: opts=%s reqs=%s\n"
			   (make-string indent ? ) i
			   (sgml-untangel-moves (sgml-state-opts (car x)))
			   (sgml-untangel-moves (sgml-state-reqs (car x))))))
	   (t				; and-node
	    (princ (format "%s%d: and-node next=%d\n"
			   (make-string indent ? ) i
			   (sgml-code-xlate (sgml-and-node-next (car x)))))
	    (loop for m in (sgml-and-node-dfas (car x))
		  do (sgml-dp-model m (+ indent 2))))))))

(defun sgml-untangel-moves (moves)
  (loop for m in moves
	collect (list (sgml-move-token m)
		      (sgml-code-xlate (sgml-move-dest m)))))


;;;; Dump state

(defun sgml-dump-state ()
  (interactive)
  (with-output-to-temp-buffer "*State dump*"
    (sgml-dp-state sgml-current-state)))

(defun sgml-dp-state (state &optional indent)
  (or indent (setq indent 0))
  (cond
   ((sgml-normal-state-p state)
    (sgml-dp-model state indent))
   (t
    (princ (format "%sand-state\n" (make-string indent ? )))
    (sgml-dp-state (sgml-and-state-substate state) (+ 2 indent))
    (princ (format "%s--next\n" (make-string indent ? )))
    (sgml-dp-state (sgml-and-state-next state)     (+ 2 indent))
    (princ (format "%s--dfas\n" (make-string indent ? )))
    (loop for m in (sgml-and-state-dfas state)
	  do (sgml-dp-model m (+ indent 2))
	  (princ (format "%s--\n" (make-string indent ? )))))))


;;;; Build autoloads for all interactive functions in psgml-parse

(defun sgml-build-autoloads ()
  (interactive)
  (with-output-to-temp-buffer "*autoload*"
    (loop
     for file in '("psgml-parse" "psgml-edit" "psgml-dtd"
		   "psgml-info" "psgml-charent")
     do
     (set-buffer (find-file-noselect (concat file ".el")))
     (goto-char (point-min))
     (while (and
	     (not (eobp))
	     (re-search-forward "^(defun +\\([^ ]+\\)" nil t))
       (let ((name (buffer-substring (match-beginning 1)
				     (match-end 1)))
	     doc)
	 (forward-sexp 1)		; skip argument list
	 (skip-chars-forward " \n\t")
	 (when (eq ?\" (following-char)) ; doc string
	       (setq doc (buffer-substring (point)
					   (progn (forward-sexp 1)
						  (point)))))
	 (skip-chars-forward " \n\t")
	 (when (looking-at "(interactive")
	       (if (null doc)
		   (message "No doc for %s" name))
	       (princ (format
		       "(autoload '%s \"%s\" %s t)\n"
		       name file doc))))))))

;;;; Test psgml with sgmls test cases

(defun test-sgml (start)
  (interactive "p")
  (let (file
	(sgml-show-warnings t))
    (with-output-to-temp-buffer "*Testing psgml*"
      (while
	  (progn
	    (setq file (format "/u2/src/sgmls-1.1/test/test%03d.sgm"
			       start))
	    (file-exists-p file))
	(princ (format "*** File test%03d ***\n" start))
	(find-file file)
	(condition-case errcode
	    (progn
	      (sgml-parse-prolog)
	      ;;(sgml-next-trouble-spot)
	      (sgml-parse-until-end-of nil))
	  (error
	   (princ errcode)
	   (terpri)))
	(if (get-buffer sgml-log-buffer-name)
	    (princ (save-excursion
		     (set-buffer sgml-log-buffer-name)
		     (buffer-string))))
	(terpri)
	(terpri)
	(sit-for 0)
	(kill-buffer (current-buffer))
	(setq start (1+ start))))))


;;;; Profiling

(defun profile-sgml (&optional file)
  (interactive)
  (or file (setq file (expand-file-name "~/work/sigmalink/BBB/config/configspec.xml")))
  (find-file file)
  (sgml-need-dtd)
  (sgml-instrument-parser)
  (elp-reset-all)
  (dotimes (i 5)
    (garbage-collect)
    (sgml-reparse-buffer (function sgml-handle-shortref)))
  (elp-results))

(defun sgml-instrument-parser ()
  (interactive)
  (require 'elp)
  (setq elp-function-list nil)
  (elp-restore-all)
  (setq elp-function-list
	'(
	  sgml-parse-to
	  sgml-parser-loop
	  sgml-parse-markup-declaration
	  sgml-do-processing-instruction
	  sgml-pop-entity
	  sgml-tree-net-enabled
	  sgml-do-end-tag
	  sgml-do-data
	  sgml-deref-shortmap
	  sgml-handle-shortref
	  sgml-do-start-tag
	  sgml-do-general-entity-ref
	  sgml-set-face-for
	  sgml-pcdata-move
	  sgml-shortmap-skipstring
	  ;;
	  sgml-parse-attribute-specification-list
	  sgml-check-tag-close
	  sgml-do-move
	  sgml-open-element
	  sgml-list-implications
	  sgml-move-current-state
          sgml-do-empty-start-tag
          sgml-lookup-eltype
          sgml-startnm-char-next
          sgml-eltype-defined
          sgml-execute-implied
          sgml-next-sub-and
          sgml-get-and-move
          format
	  ))
  (elp-instrument-list))


(defun sgml-instrument-dtd-parser ()
  (interactive)
  (require 'elp)
  (setq elp-function-list nil)
  (elp-restore-all)
  (setq elp-function-list
	'(
	  sgml-parse-prolog
	  sgml-skip-ds
	  sgml-parse-markup-declaration
	  sgml-check-doctype-body
	  ;;
	  sgml-check-dtd-subset
	  sgml-parse-ds
	  sgml-declare-attlist
	  sgml-declare-entity
	  sgml-declare-element
	  sgml-declare-shortref
	  ;;
	  sgml-parse-parameter-literal
	  sgml-check-element-type
	  sgml-check-primitive-content-token
	  sgml-check-model-group
	  ;; In sgml-check-model-group
	  sgml-parse-modifier
	  sgml-make-pcdata
	  sgml-skip-ts
	  sgml-make-opt
	  sgml-make-*
	  sgml-make-+
	  sgml-reduce-,
	  sgml-reduce-|
	  sgml-make-&
	  sgml-make-conc
	  sgml-copy-moves
	  ;; is ps*
	  sgml-do-parameter-entity-ref
	  ;;
	  sgml-make-primitive-content-token
	  sgml-push-to-entity
	  sgml-lookup-entity
	  sgml-lookup-eltype
	  sgml-one-final-state
	  sgml-remove-redundant-states-1
	  ))
  (elp-instrument-list))

;\end{codeseg}
