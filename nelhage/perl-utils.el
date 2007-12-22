;; -*- mode: emacs-lisp; indent-tabs-mode: nil -*-
(require 'structured)
(require 'perl-find-library)
(require 'cperl-mode)

(defun perl-insert-brackets (&optional arg)
  (interactive "P")
  (let ((multiline
         ;;If we have no argument, try to guess whether we should
         ;;insert the brackets on the same line (e.g. $foo->{...}), or
         ;;on different lines, e.g:
         ;; if (...) {
         ;;    ...
         ;; }
         (save-excursion
           (let ((point (point)))
             (beginning-of-line)
             (while (looking-at "[ \t]*$")
               (forward-line -1))
             (re-search-forward "[ \t}]*")
             (cond ((looking-at (regexp-opt '("sub" "eval" "do" "else") t)) t)
                   ((looking-at (regexp-opt '("for" "foreach" "while" "if" "elsif") t))
                    ;; These are harder, since they may contain code on
                    ;; the same line as the keyword, which may contain,
                    ;; e.g. hashref accesses.
                    (beginning-of-line)
                    (forward-word) ;;Skip the keyword
                    ;; There's a paren here
                    (and (looking-at "[^\n]*(")
                         (progn
                           (re-search-forward "(")
                           (backward-char 1)
                           ;;We're now before the paren
                           (condition-case nil
                               (progn
                                 (forward-sexp)
                                 t)
                             ('scan-error . nil)))
                         ;;If, after we find the matching paren, we
                         ;;are *before* the insertion point, e.g.
                         ;;
                         ;; if ( ... ) --><--
                         ;;
                         ;; Then we want to insert newlines. If we're *after*, then
                         ;; the insertion point is inside the parens, so no newline.
                         (<= (point) point))))))))

    (insert-brackets
     (cond (arg arg)
           (multiline arg)
           (t '(()))))))

(defun perl-add-test (&optional count)
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
         (rx line-start
             "use" (1+ (any space)) (one-or-more (or (syntax word) ":")) "::Test"
             (1+ (any space))
             "tests"
             (1+ (any space))
             "=>"
             (1+ (any space))
             (group
              (one-or-more (any digit))))
         nil t)
        (progn
          (goto-char (match-beginning 1))
          (let ((tests (+ count (string-to-int (match-string 1)))))
            (insert (int-to-string tests))
            (let ((pt (point)))
              (forward-word)
              (delete-region pt (point)))
            (message "Tests: %d" tests)))
      (message "No test count found!"))))

(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

(defun perl-check-pod (&optional buffer)
  "Run podchecker on the given buffer"
  (interactive)
  (if (not buffer)
      (setq buffer (current-buffer)))
  (compile (concat "podchecker " (buffer-file-name))))

;;; Add podchecker errors to compilation-error-regexp-alist
(defconst *podcheck-error-regexp*
  (rx (1+ not-newline)
      (1+ (any space)) "at" (1+ (any space)) "line" (1+ (any space)) 
      (group (1+ (any digit)))
      (1+ (any space)) "in" (1+ (any space)) "file" (1+ (any space))
      (group (1+ not-newline))
      line-end))

(setq compilation-error-regexp-alist
      (append
       (list
        (list (rx-to-string `(seq line-start "*** ERROR:" (regexp ,*podcheck-error-regexp*))) 2 1)
        (list (rx-to-string `(seq line-start "*** WARNING:" (regexp ,*podcheck-error-regexp*))) 2 1 nil 1))
       compilation-error-regexp-alist))

(defun perl-guess-package (path)
  "Guess what perl package PATH contains. Searches for a 'lib'
directory in path, and, if it finds one, strips .pm off the end
and replaces directories after the /lib/ with package
names. Otherwise, just takes the last pathname component"
  (let ((lib (string-match "/lib/" path)))
    (if lib
        (replace-regexp-in-string
         "/" "::"
         (substring path (+ lib 5)
                    (- (length path) 3)))
      (progn
        (if (string-match
             (rx (group (1+ (not (any "./"))))
                 ".pm" string-end)
             path)
            (match-string 1 path)
          nil)))))

(defadvice cperl-build-manpage (before cperl-build-manpage-clear-old-page)
  "Delete the old Man buffer before running man, so that we
  regenerate it every time."
  (let ((buffer-name (concat "*Man " buffer-file-name "*")))
    (if (get-buffer buffer-name)
        (kill-buffer buffer-name))))

(ad-activate 'cperl-build-manpage)

(provide 'perl-utils)
