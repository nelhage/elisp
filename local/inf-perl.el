;;; inf-perl.el --- Perl

;; Copyright 2005 Wenbin Ye
;;
;; Author: wenbinye@163.com
;; Version: $Id: inf-perl-mode.el,v 0.0 2005/10/23 13:39:12 Administrator Exp $
;; Keywords: perl, comint
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;    This is a customisation of comint-mode (see comint.el)

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'inf-perl-mode)
;;   In windows expand-file-name is require because perl don't where is ~/psh.pl
;;   (setq perl-shell-program (expand-file-name "~/psh.pl"))

;; You may get this `psh.pl' from my homepage or write it yourself:
;; http://learn.tsinghua.edu.cn:8080/2005211356/src/psh.zip

;;; Code:

(require 'comint)
(require 'cperl-mode)

(defgroup inf-perl nil
  "*Running perlsh from whthin Emacs buffers"
  :group 'processes
  :group 'unix)

(defcustom inf-perl-prompt-pattern  "^[^#$%>\n]*[#$%>] *"
  "*Regexp to match prompts in shell"
  :type 'regexp
  :group 'inf-perl)

(defcustom inf-perl-command-regexp "[^>\n]*>+ *"
  "*Regexp to match single command"
  :type 'regexp
  :group 'inf-perl)
;; (setq inf-perl-command-regexp  "[^>\n]*>+ *")

(defcustom inf-perl-input-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :type 'regexp
  :group 'inf-perl)

(defcustom inf-perl-shell-program "re.pl"
  "*The perl shell program location"
  :type 'file
  :group 'inf-perl)

(defcustom inf-perl-start-file (expand-file-name "~/.pshhistory")
  "*The perl history file location"
  :type 'file
  :group 'inf-perl)

(defvar inf-perl-perl "perl")

(defvar inf-perl-mode-map nil)
(unless inf-perl-mode-map
  (setq inf-perl-mode-map (nconc (make-sparse-keymap)
                                         comint-mode-map)))

;; Install the process communication commands in the cperl-mode keymap
(define-key cperl-mode-map "\C-x\C-e" 'inf-perl-send-line)
(define-key cperl-mode-map "\C-c\C-r" 'inf-perl-send-region)
(define-key cperl-mode-map "\C-c\M-r" 'inf-perl-send-region-and-go)
(define-key cperl-mode-map "\C-c\C-i" 'inf-perl-switch-to-perl)

(defvar inf-perl-buffer nil "*The current perl process buffer.")

(defcustom inf-perl-mode-hook '()
  "Hook for customising inf-perl mode"
  :type 'hook
  :group 'inf-perl)

(defvar inf-perl-font-lock-keywords)

(put 'inf-perl-mode 'mode-class 'special)

(define-derived-mode inf-perl-mode comint-mode "Perl-Interaction"
  "In cperl-mode, you can send text to the inferior perl process.
     inf-perl-switch-to-perl   switches to perl process buffer
     inf-perl-send-line   send current line to the perl process
     inf-perl-send-region send the current region to the perl process
     inf-perl-send-region-and-go  send the current region to the perl process
                              and switch to the perl process buffer

Something uncomfortable is that, the subroutine can't redefine in perl.
So the provided perl shell program translate the code \"sub func { body }\"
to \"*func = sub { body };\". Evaluate this code has a warnning, but it
does work."

  (setq comint-prompt-regexp "[^>\n]*>+ *")
  (setq comint-prompt-regexp inf-perl-prompt-pattern)
  (setq comint-input-filter (function inf-perl-input-filter)))

(defun inf-perl-input-filter (str)
  "Don't save anything matching `inf-perl-input-filter-regexp'."
  (not (string-match inf-perl-input-filter-regexp str)))

(defalias 'run-perl 'inf-perl-start)
(defun inf-perl-start (&optional buffer)
  "Run an inferior perl process, input and output via buffer *perl*.
If there is a process already running in `*perl*', switch to that buffer."
  (interactive)
  (setq buffer (get-buffer-create (or buffer "*perl*")))
  (setq inf-perl-buffer buffer)
  (pop-to-buffer buffer)
  (unless (comint-check-proc buffer)
    (let ((prog inf-perl-perl))
      (apply 'make-comint-in-buffer "perl" buffer prog
             (if (file-exists-p inf-perl-start-file) inf-perl-start-file)
             (list inf-perl-shell-program))
      (inf-perl-mode)))
  buffer)

(defun inf-perl-send-region (start end)
  "Send region to perl interaction buffer"
  (interactive "r")
  (let ((string (buffer-substring-no-properties start end)))
    (setq string
          (replace-regexp-in-string "\n" "\\\n" string nil '\\))
    (comint-send-string (inf-perl-proc) (concat string "\n"))))

(defun inf-perl-proc ()
  "Return the current perl process. See variables `inf-perl-buffer'."
  (let ((proc (get-buffer-process
               (if (eq major-mode 'inf-perl-mode)
                   (current-buffer)
                 inf-perl-buffer))))
    (or proc
        (error "No current process. See variables `inf-perl-buffer'"))))

(defun inf-perl-send-region-and-go (start end)
  "Send region and switch to process buffer"
  (interactive "r")
  (inf-perl-send-region start end)
  (inf-perl-switch-to-perl t))

(defun inf-perl-switch-to-perl (eob-p)
  "Switch to perl process buffer.
With argument, position current at end of buffer."
  (interactive "P")
  (if (get-buffer inf-perl-buffer)
      (pop-to-buffer inf-perl-buffer)
    (error "No current process. See variables `inf-perl-buffer'"))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun inf-perl-send-line ()
  "Send current line to the perl process."
  (interactive)
  (inf-perl-send-region
   (line-beginning-position)
   (line-end-position)))

(provide 'inf-perl)
(eval-when-compile
  (require 'cl))

;;; inf-perl.el ends here
