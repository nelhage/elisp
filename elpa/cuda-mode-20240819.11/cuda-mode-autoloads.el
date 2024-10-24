;;; cuda-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from cuda-mode.el

(add-to-list 'auto-mode-alist '("\\.cu[h]?\\'" . cuda-mode))
(autoload 'cuda-mode "cuda-mode" "\
Major mode for editing Cuda code.
This mode derives from C++ mode.
Key bindings:
\\{ccuda-mode-map}

(fn)" t)
(register-definition-prefixes "cuda-mode" '("cuda-"))

;;; End of scraped data

(provide 'cuda-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; cuda-mode-autoloads.el ends here
