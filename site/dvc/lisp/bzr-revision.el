;;; bzr-revision.el --- Management of revision lists in bzr

;; Copyright (C) 2006 - 2008  by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Contributions from:
;;    Stefan Reichoer, <stefan@xsteve.at>
;; Keywords:

;; DVC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; DVC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;

;;; Code:

(require 'dvc-revlist)

(eval-when-compile (require 'cl))

(defstruct (bzr-revision-st)
  revno
  message
  creator
  branch-nick
  date
  merges
  )

;; bzr revision list

(defun bzr-revision-list-entry-patch-printer (elem)
  "TODO"
  (insert (if (dvc-revlist-entry-patch-marked elem)
              (concat " " dvc-mark " ") "   "))
  (let ((struct (dvc-revlist-entry-patch-struct elem)))
    (insert (dvc-face-add "revno: " 'dvc-header)
            (dvc-face-add (int-to-string (or (bzr-revision-st-revno struct) -99))
                          'dvc-revision-name)
            "\n")
    (when dvc-revisions-shows-creator
      (insert "   " (dvc-face-add "committer: " 'dvc-header)
              (or (bzr-revision-st-creator struct) "?") "\n"))
    (when dvc-revisions-shows-date
      (insert "   " (dvc-face-add "timestamp: " 'dvc-header)
              (or (bzr-revision-st-date struct) "?") "\n"))
    (insert "   " (dvc-face-add "branch nick: " 'dvc-header)
            (or (bzr-revision-st-branch-nick struct) "?") "\n")
    (when dvc-revisions-shows-summary
      (insert "   " (dvc-face-add "message: " 'dvc-header)
              (or (bzr-revision-st-message struct) "?") "\n"))
    ))

;;; bzr log
(defun bzr-log-parse-remote (log-buffer location)
  (bzr-log-parse log-buffer location t))

(defun bzr-missing-parse (log-buffer location)
  "Parse the output of bzr missing."
  (bzr-log-parse log-buffer location nil t))

(defun bzr-log-parse (log-buffer location &optional remote missing)
  "Parse the output of bzr log."
  ;;(dvc-trace "location=%S" location)
  (goto-char (point-min))
  (let ((root location)
        (intro-string)
        (brief (with-current-buffer log-buffer dvc-revlist-brief)))
    (when missing ;; skip the first status output
      (unless (re-search-forward "^------------------------------------------------------------$" nil t)
        (message "No missing revisions: Branches are up to date.")
        (goto-char (point-max)))
      (setq intro-string (buffer-substring-no-properties (point-min) (point)))
      (with-current-buffer log-buffer
        (let ((buffer-read-only nil))
          (insert intro-string))))
    (while (> (point-max) (point))
      (forward-line 1)
      (let ((start (point))
            (message-start-pos)
            (message-end-pos)
            (elem (make-bzr-revision-st)))
        (or (and (re-search-forward
                  "^------------------------------------------------------------$"
                  nil t)
                 (progn (beginning-of-line)
                        t))
            (goto-char (point-max)))
        (save-restriction
          (save-excursion
            (narrow-to-region start (- (point) 1))
            ;;(dvc-trace "parsing %S" (buffer-string))
            (goto-char (point-min))
            (while (re-search-forward "^\\([a-z][a-z ]*[a-z]\\):\\( \\|\n\\)" nil t)
              ;;(dvc-trace "match-string=%S" (match-string 1))
              (cond ((string= (match-string 1) "revno")
                     (setf (bzr-revision-st-revno elem)
                           (string-to-number
                            (buffer-substring-no-properties
                             (point) (line-end-position)))))
                    ((string= (match-string 1) "committer")
                     (setf (bzr-revision-st-creator elem)
                           (buffer-substring-no-properties
                            (point) (line-end-position))))
                    ((string= (match-string 1) "branch nick")
                     (setf (bzr-revision-st-branch-nick elem)
                           (buffer-substring-no-properties
                            (point) (line-end-position))))
                    ((string= (match-string 1) "timestamp")
                     (setf (bzr-revision-st-date elem)
                           (buffer-substring-no-properties
                            (point) (line-end-position))))
                    ((string= (match-string 1) "message")
                     ;;(dvc-trace "found message")
                     (re-search-forward "^[ \t]*")
                     (setq message-start-pos (point))
                     (setq message-end-pos
                           (if brief
                               (line-end-position)
                             (if (re-search-forward "^--------" nil t) (point) (point-max))))
                     (setf (bzr-revision-st-message elem)
                           (buffer-substring-no-properties
                            message-start-pos message-end-pos))
                     (goto-char (point-max)))
                    (t (dvc-trace "unmanaged field %S" (match-string 1))))
              (forward-line 1)
              (beginning-of-line))))
        (forward-line 1)
        (with-current-buffer log-buffer
          (ewoc-enter-last
           dvc-revlist-cookie
           `(entry-patch
             ,(make-dvc-revlist-entry-patch
               :dvc 'bzr
               :struct elem
               :rev-id `(bzr (revision
                              ,(list (if remote 'remote 'local)
                                     root (bzr-revision-st-revno
                                           elem)))))))
          (goto-char (point-min))
          (dvc-revision-prev))))))

(defun bzr-log-refresh ()
  "Refresh a log buffer."
  (let ((cmd (remove
              nil
              (append
               (list "log")
               (if dvc-revlist-last-n
                   (list "-r" (format "last:%d.." dvc-revlist-last-n)))
               (list dvc-revlist-path)))))
    (dvc-build-revision-list
     'bzr 'alog default-directory cmd 'bzr-log-parse
     dvc-revlist-brief dvc-revlist-last-n dvc-revlist-path
     'bzr-log-refresh))
  (goto-char (point-min)))

;;;###autoload
(defun bzr-log (path last-n)
  "Run bzr log for PATH and show only the first line of the log message.
LAST-N revisions are shown (default dvc-log-last-n). Note that the
LAST-N restriction is applied first, so if both PATH and LAST-N are
specified, fewer than LAST-N revisions may be shown."
  (interactive (list default-directory (if current-prefix-arg (prefix-numeric-value current-prefix-arg) dvc-log-last-n)))
  (let ((default-directory (bzr-branch-root (or path default-directory)))
        (dvc-revlist-path path)
        (dvc-revlist-brief t)
        (dvc-revlist-last-n last-n))
    (bzr-log-refresh)))

;;;###autoload
(defun bzr-log-remote (location)
  "Run bzr log against a remote location."
  (interactive (list (read-string "Location of the branch: ")))
  (dvc-build-revision-list 'bzr 'remote-log location `("log" ,location)
                           'bzr-log-parse-remote t nil nil
                           (dvc-capturing-lambda ()
                             (bzr-log-remote (capture location))))
  (goto-char (point-min)))

;;;###autoload
(defun bzr-changelog (&optional path)
  "Run bzr log and show the full log message."
  (interactive (list default-directory))
  (let ((default-directory (bzr-branch-root (or path default-directory))))
    (dvc-build-revision-list 'bzr 'alog default-directory '("log") 'bzr-log-parse nil nil path
                             (dvc-capturing-lambda ()
                               (bzr-changelog (capture path))))
    (goto-char (point-min))))

;;;###autoload
(defun bzr-dvc-missing (&optional other)
  "Run bzr missing."
  (interactive "sBzr missing against other: ")
  (when (string= other "")
    (setq other nil))
  ;;(message "bzr-dvc-missing %S" other)
  (dvc-build-revision-list 'bzr 'missing (bzr-tree-root)
                           `("missing" ,other)
                           'bzr-missing-parse
                           nil nil nil
                           (dvc-capturing-lambda ()
                             (bzr-dvc-missing (capture other))))
  (goto-char (point-min)))

(provide 'bzr-revision)
;;; bzr-revision.el ends here
