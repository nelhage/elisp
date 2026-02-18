(defun replace-all (from to str)
  "Replace all instances of FROM with TO in STR, and return the
result"
  (while (string-match from str)
    (setq str (replace-match to t t str)))
  str)

(defmacro indenting-changes (&rest body)
  "Execute BODY and then indent any insertions made at point"
  `(let ((indent-region-start (point)))
       ,@body
       (indent-region indent-region-start (point) nil)
       (indent-according-to-mode)))

(defun beginning-of-line-dwim ()
  "Execute `beginning-of-line-text', or `beginning-of-line' if point
is already at the beginning of the text."
  (interactive)
  (let ((pt (point)))
        (beginning-of-line-text)
        (if (= pt (point))
                (beginning-of-line))))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun increment-number-at-point (&optional amount)
  "Increment the number under point by `amount'"
  (interactive "p")
  (let ((num (number-at-point)))
    (when (numberp num)
      (let ((newnum (+ num amount))
            (p (point)))
        (save-excursion
          (skip-chars-backward "-.0123456789")
          (delete-region (point) (+ (point) (length (number-to-string num))))
          (insert (number-to-string newnum)))
        (goto-char p)))))

(defun decrement-number-at-point (&optional amount)
  "Decrement the number under point by `amount'"
  (interactive "p")
  (increment-number-at-point (- (abs amount))))

(defun kill-buffers-under (path)
  "Kill all buffers visiting files under PATH."
  (interactive "DKill buffers under directory: ")
  (let ((path (file-truename (expand-file-name path)))
        (killed 0))
    (dolist (buf (buffer-list))
      (when-let ((file (buffer-file-name buf)))
        (when (string-prefix-p path (file-truename file))
          (kill-buffer buf)
          (cl-incf killed))))
    (message "Killed %d buffer%s under %s" killed (if (= killed 1) "" "s") path)))

(provide 'utils)
