;;; lsp-bufls.el --- bufls-langserver Client settings -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Jim Myhrberg

;; Author: Jim Myhrberg
;; Keywords: lsp, protobuf, buf, bufls

;; This file is not part of GNU Emacs

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; lsp-bufls client

;;; Code:

(require 'lsp-mode)
(require 'lsp-go)

(defgroup lsp-bufls nil
  "Configuration options for lsp-bufls."
  :group 'lsp-mode
  :link '(url-lint "https://github.com/bufbuild/buf-language-server")
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-bufls-args nil
  "Arguments to pass to bufls serve."
  :type '(repeat string)
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-bufls-path "bufls"
  "Command to run bufls."
  :type 'string
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp-bufls-server--stdio-command ()
  "Return the command and args to start bufls-langserver."
  (let ((args (list lsp-bufls-path "serve")))
    (when (and (listp lsp-bufls-args)
               (> (length lsp-bufls-args) 0))
      (setq args (append args lsp-bufls-args)))
    args))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   #'lsp-bufls-server--stdio-command)
                  :activation-fn (lsp-activate-on "protobuf")
                  :language-id "protobuf"
                  :priority 0
                  :server-id 'bufls))

(lsp-consistency-check lsp-bufls)

(provide 'lsp-bufls)
;;; lsp-bufls.el ends here
