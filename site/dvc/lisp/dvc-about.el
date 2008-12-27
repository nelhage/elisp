;;; dvc-about.el --- "About DVC" message

;; Copyright (C) 2006 by all contributors

;; This file is part of DVC.
;;
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

;; Eye cather displaying about DVC

;;; Code:

(eval-when-compile (require 'cl))
(require 'dvc-buffers)
(require 'dvc-version)

;; Test cases
;; (dvc-about-message-with-bouncing
;;  (concat "Author: Stefan Reichoer <stefan@xsteve.at>, "
;;          "Contributions from: "
;;          "Matthieu Moy <Matthieu.Moy@imag.fr>, "
;;          "Masatake YAMATO <jet@gyve.org>, "
;;          "Milan Zamazal <pdm@zamazal.org>, "
;;          "Martin Pool <mbp@sourcefrog.net>, "
;;          "Robert Widhopf-Fenk <hack@robf.de>, "
;;          "Mark Triggs <mst@dishevelled.net>"))
;; (dvc-about-message-with-rolling
;;  (concat "Author: Stefan Reichoer <stefan@xsteve.at>, "
;;          "Contributions from: "
;;          "Matthieu Moy <Matthieu.Moy@imag.fr>, "
;;          "Masatake YAMATO <jet@gyve.org>, "
;;          "Milan Zamazal <pdm@zamazal.org>, "
;;          "Martin Pool <mbp@sourcefrog.net>, "
;;          "Robert Widhopf-Fenk <hack@robf.de>, "
;;          "Mark Triggs <mst@dishevelled.net>"))
(defvar dvc-about-message-long-default-interval 0.2
  "Default animation step interval.

Used in `dvc-about-message-with-bouncing' and `dvc-about-message-with-rolling'")

(defvar dvc-about-message-long-border-interval 1.0
  "Animation step interval when bouncing in `dvc-about-message-with-bouncing'.")

(defun* dvc-about-message-with-bouncing (&rest msg)
  "Similar to `message' but display the message in bouncing animation to show long line."
  (setq msg (apply 'format msg))
  (let* ((width (- (window-width (minibuffer-window))
                   (+ 1 (length "[<] ") (length " [>]"))))
         (msglen (length msg))
         submsg
         (steps (- msglen width))
         j)
    (if (< msglen width)
        (message "%s" msg)
      (while t
        ;; Go forward
        (dotimes (i steps)
          (setq submsg (substring msg i (+ i width)))
          (message "[<] %s [ ]" submsg)
          (unless (sit-for (cond
                            ((eq i 0) dvc-about-message-long-border-interval)
                            (t dvc-about-message-long-default-interval)))
            (return-from dvc-about-message-with-bouncing)))
        ;; Go back
        (dotimes (i steps)
          (setq j (- steps i))
          (setq submsg (substring msg j (+ j width)))
          (message "[ ] %s [>]" submsg)
          (unless (sit-for (cond
                            ((eq i 0) dvc-about-message-long-border-interval)
                            (t dvc-about-message-long-default-interval)))
            (return-from dvc-about-message-with-bouncing)))
        (garbage-collect)))))

(defun* dvc-about-message-with-rolling (&rest msg)
  "Similar to `message' but display the message in rolling animation to show long line."
  (setq msg (concat "  <MESSAGE>: "
                    (apply 'format msg)
                    "            "))
  (let* ((width (- (window-width (minibuffer-window))
                   (+ 1 (length "[<] "))))
         (msglen (length msg))
         submsg
         (normal-range (- msglen width)))
    (if (< msglen width)
        (message "%s" msg)
      (while t
        (dotimes (i msglen)
          (setq submsg (if (< i normal-range)
                           (substring msg i (+ i width))
                         ;; Rolling is needed.
                         (concat (substring msg i)
                                 (substring msg 0 (- (+ i width) msglen)))))
          (message "[<] %s" submsg)
          (unless (sit-for (cond
                            ((eq i 0) dvc-about-message-long-border-interval)
                            (t dvc-about-message-long-default-interval)))
            (return-from dvc-about-message-with-rolling)))
        (garbage-collect)))))

;;;###autoload
(defun dvc-about ()
  "Displays a welcome message."
  (interactive)
  (let* ((name "*dvc-welcome*")
         (buffer (get-buffer name)))
    (if buffer (dvc-switch-to-buffer buffer)
      (dvc-switch-to-buffer
       (setq buffer (get-buffer-create name)))
      (insert "               *** Welcome to DVC ! *** \n")
      (insert "\n")
      (insert (format "DVC version: %s" dvc-version))
      (insert "\n")
      (insert
       "\n"
       ""
       "[" (dvc-about-insert-button "About" 'dvc-about)
       "]"
       "\n")
      (toggle-read-only t)
      (local-set-key [?q] (lambda () (interactive)
                            (kill-buffer (current-buffer)))))
    ;; TODO: Use CONTRIBUTORS file.
    (dvc-about-message-with-bouncing
     (concat "Author: Stefan Reichoer <stefan@xsteve.at>, "
             "Contributions from: "
             "Matthieu Moy <Matthieu.Moy@imag.fr>, "
             "Masatake YAMATO <jet@gyve.org>, "
             "Milan Zamazal <pdm@zamazal.org>, "
             "Martin Pool <mbp@sourcefrog.net>, "
             "Robert Widhopf-Fenk <hack@robf.de>, "
             "Mark Triggs <mst@dishevelled.net>"
             "WE MUST UPDATE THIS LIST"))))

(defun dvc-about-insert-button (label function)
  "Insert a button labeled with LABEL and launching FUNCTION.
Helper function for `dvc-about'."
  (dvc-face-add label 'bold
                (let ((map (make-sparse-keymap)))
                  (define-key map [return]  function)
                  (define-key map "\C-m"    function)
                  (define-key map [mouse-2] function)
                  map)
                nil))

(provide 'dvc-about)
;; Local Variables:
;; End:

;;; dvc-about.el ends here
