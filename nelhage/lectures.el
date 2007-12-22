(defvar *lecture-notes-dir* (expand-file-name "~/mit/"))

(defun read-lecture-class ()
  (let ((classes (directory-files
				  *lecture-notes-dir*
				  nil
				  "^[0-9]+\\.[0-9]+[A-Z]?$")))
	(completing-read "Class: " classes
					 nil nil nil nil)))

(defun start-lecture-notes (&optional class)
  (interactive)
  (if (null class)
	  (setq class (read-lecture-class)))
  (let* ((date (format-time-string "%Y-%m-%d"))
		 (path (format "%s/%s/" *lecture-notes-dir* class))
		 (file (format "%s/LN-%s.txt" path date)))
	(make-directory path t)
	(find-file file)
	(insert "Lecture notes " date "\n"
			"------------------------\n")
	(set-buffer-modified-p nil)
	(auto-fill-mode)))
