;; Interactive selection of base16 themes
;; This comment here mostly so I can see what comments look like.

(defun read-from-file (fname)
  (with-temp-buffer
    (insert-file-contents fname)
    (goto-char 0)
    (read (current-buffer))))

(defvar base16--all-themes (read-from-file (expand-file-name "~/.elisp/base16.el")))
(defvar base16--theme-list base16--all-themes)
(defvar base16--theme-idx 0)
(defvar base16--liked-themes '())


(defun base16--active-theme ()
  (nth base16--theme-idx base16--theme-list))

(defun base16-next-theme (&optional arg)
  (interactive "p")
  (setq base16--theme-idx (mod (+ base16--theme-idx arg) (length base16--theme-list)))
  (let ((new-theme (base16--active-theme)))
    (message "Setting theme: %s (%d/%d)" new-theme base16--theme-idx (length base16--theme-list))
    (load-theme new-theme t)))

(defun base16-prev-theme ()
  (interactive)
  (base16-next-theme -1))

(defun base16-like-theme ()
  (interactive)
  (let ((theme (base16--active-theme)))
    (if (not (memq theme base16--liked-themes))
        (progn
          (push theme base16--liked-themes)
          (message "Added to liked: %s" theme))
      (setq base16--liked-themes (delq theme base16--liked-themes))
      (message "Removed from liked: %s" theme))))

(define-key global-map (kbd "C-x .") 'base16-next-theme)
(define-key global-map (kbd "C-x ,") 'base16-prev-theme)
(define-key global-map (kbd "C-x !") 'base16-like-theme)


(defun base16-save-liked ()
  (interactive)
  (with-temp-buffer
    (insert (format "%S" base16--liked-themes))
    (write-file (expand-file-name "~/.elisp/liked-themes.el"))
    (message "Saved %s theems" (length base16--liked-themes))))
