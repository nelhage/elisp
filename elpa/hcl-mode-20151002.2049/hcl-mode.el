;;; hcl-mode.el --- Major mode for Hashicorp

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-hcl-mode
;; Package-Version: 20151002.2049
;; Version: 0.01
;; Package-Requires: ((cl-lib "0.5"))

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

;; hcl-mode provides a major-mode of hcl file.

;;; Code:

(require 'cl-lib)
(require 'rx)

(defgroup hcl nil
  "Major mode of Hashicorp Configuration Language."
  :group 'languages)

(defcustom hcl-indent-level 2
  "The tab width to use when indenting."
  :type 'integer
  :group 'hcl)

(defconst hcl--block-regexp
  "^\\s-*[^{]+{")

;; String Interpolation(This regexp is taken from ruby-mode)
(defconst hcl--string-interpolation-regexp
  "\\${[^}\n\\\\]*\\(?:\\\\.[^}\n\\\\]*\\)*}")

(defconst hcl--assignment-regexp
  "\\s-*\\([[:word:]]+\\)\\s-*=\\(?:[^>=]\\)")

(defconst hcl--map-regexp
  "\\s-*\\([[:word:]]+\\)\\s-*{")

(defconst hcl--boolean-regexp
  (concat "\\(?:^\\|[^.]\\)"
          (regexp-opt '("true" "false" "on" "off" "yes" "no")
                      'words)))

(defvar hcl-font-lock-keywords
  `((,hcl--assignment-regexp 1 font-lock-variable-name-face)
    (,hcl--boolean-regexp . font-lock-constant-face)
    (,hcl--map-regexp 1 font-lock-type-face)
    (,hcl--string-interpolation-regexp 0 font-lock-variable-name-face t)))

(defsubst hcl--paren-level ()
  (car (syntax-ppss)))

(defsubst hcl--in-string-or-comment-p ()
  (nth 8 (syntax-ppss)))

(defun hcl--block-indentation ()
  (let ((curline (line-number-at-pos)))
    (save-excursion
      (condition-case nil
          (progn
            (backward-up-list)
            (unless (= curline (line-number-at-pos))
              (current-indentation)))
        (scan-error nil)))))

(defun hcl--previous-indentation ()
  (save-excursion
    (forward-line -1)
    (let (finish)
      (while (not finish)
        (cond ((bobp) (setq finish t))
              ((hcl--in-string-or-comment-p) (forward-line -1))
              (t
               (let ((line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))))
                 (if (not (string-match-p "\\`\\s-*\\'" line))
                     (setq finish t)
                   (forward-line -1))))))
      (current-indentation))))

(defun hcl-indent-line ()
  "Indent current line as Hcl configuration."
  (interactive)
  (let* ((curpoint (point))
         (pos (- (point-max) curpoint)))
    (back-to-indentation)
    (if (hcl--in-string-or-comment-p)
        (goto-char curpoint)
      (let ((block-indentation (hcl--block-indentation)))
        (delete-region (line-beginning-position) (point))
        (if block-indentation
            (if (looking-at "[]}]")
                (indent-to block-indentation)
              (indent-to (+ block-indentation hcl-indent-level)))
          (indent-to (hcl--previous-indentation)))
        (when (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos)))))))

(defun hcl-beginning-of-defun (&optional count)
  (interactive "p")
  (setq count (or count 1))
  (let ((match 0)
        finish)
    (while (and (not finish)
                (re-search-backward hcl--block-regexp nil t))
      (unless (hcl--in-string-or-comment-p)
        (cl-incf match)
        (when (= match count)
          (setq finish t))))))

(defun hcl-end-of-defun (&optional count)
  (interactive "p")
  (let ((paren-level (hcl--paren-level)))
    (when (or (and (looking-at-p "}") (= paren-level 1))
              (= paren-level 0))
      (re-search-forward hcl--block-regexp nil t)))
  (dotimes (_i count)
    (when (looking-at-p hcl--block-regexp)
      (goto-char (line-end-position)))
    (hcl-beginning-of-defun 1)
    (skip-chars-forward "^{")
    (forward-char 1)
    (let ((orig-level (hcl--paren-level)))
      (while (>= (hcl--paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-line +1)))))

(defvar hcl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-a") 'hcl-beginning-of-defun)
    (define-key map (kbd "C-M-e") 'hcl-end-of-defun)
    map)
  "Keymap for Hcl major mode.")

;;;###autoload
(define-derived-mode hcl-mode prog-mode "HCL"
  "Major mode for editing hcl configuration file"

  (setq font-lock-defaults '((hcl-font-lock-keywords)))

  (modify-syntax-entry ?_ "w" hcl-mode-syntax-table)

  ;;; Comments
  ;; Single line comment
  (modify-syntax-entry ?# "< b" hcl-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" hcl-mode-syntax-table)

  ;; multiple line comment(/* ... */) taken from `go-mode'
  (modify-syntax-entry ?/  ". 124b" hcl-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23" hcl-mode-syntax-table)

  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")

  ;; indentation
  (make-local-variable 'hcl-indent-level)
  (set (make-local-variable 'indent-line-function) 'hcl-indent-line)

  (set (make-local-variable 'beginning-of-defun-function)
       'hcl-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'hcl-end-of-defun)

  ;; electric-mode
  (set (make-local-variable 'electric-indent-chars)
       (append "{}[]" electric-indent-chars)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hcl\\'" . hcl-mode))

(provide 'hcl-mode)

;;; hcl-mode.el ends here
