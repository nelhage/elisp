(defun insert-close (&optional char)
  "Insert the closing bracket CHAR, or else the value of
`last-command-event'. If there is already one after
point, (optionally after whitespace), move to just in front of
it."
  (interactive)
  (if (null char) (setq char last-command-event))
  (cond ((and (stringp char) (= (length char) 1))
		 (setq char (elt char 0)))
		((not (integerp char))
		 (error "Not a character: " char)))
  (if (looking-at (concat "[\\ s\t\n]*" (regexp-quote (char-to-string char))))
      (goto-char (match-end 0))
      (insert char)))

(defun insert-brackets (&optional skip-lines)
  "Insert {}, with a blank line in between, and put the point on
the blank line. Indent all three lines for the current mode. With
prefix argument, insert the brackets on a single line, with a
space between them. With a numeric prefix arg, place the closing
bracket after that many lines of existing text."
  (interactive "P")
  (let ((start (save-excursion
                 (beginning-of-line)
                 (point))))
    (insert "{")
    (cond ((numberp skip-lines)
           (progn
             (forward-line skip-lines)
             (end-of-line)
             (insert "\n}")))
          (skip-lines (insert "}"))
          (t (insert "\n\n}")))
    (indent-region start (point) nil)
    (cond ((numberp skip-lines)
           (forward-line (- skip-lines)))
          (skip-lines (backward-char))
          (t (forward-line -1)))
    (unless (and skip-lines (not (numberp skip-lines)))
      (beginning-of-line)
      (indent-according-to-mode))))

(defun unwrap-next-sexp (&optional kill-n-sexps)
  "Convert (x ...)  to ..."
  (interactive "P")
  (forward-sexp)
  (backward-delete-char 1)
  (backward-up-list)
  (delete-char 1)
  (when kill-n-sexps
    (kill-sexp kill-n-sexps)))

(defun backspace-unwrap-sexp (&optional arg)
  "Delete one character backwards. If the character deleted is an
open parenthesis, delete the corresponding close as well."
  (interactive "P")
  (backward-char)
  (if (and (not arg) (looking-at "("))
	  (unwrap-next-sexp)
	(delete-char 1)))

(provide 'structured)
