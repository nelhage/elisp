;;; cuda-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/cuda-mode-20201013.2230/cuda-mode"
;;;;;;  "../../../.emacs.d/elpa/cuda-mode-20201013.2230/cuda-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/cuda-mode-20201013.2230/cuda-mode.el

(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))

(autoload 'cuda-mode "../../../.emacs.d/elpa/cuda-mode-20201013.2230/cuda-mode" "\
Major mode for editing CUDA.
Cuda is a C like language extension for mixed native/GPU coding
created by NVIDIA

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `cuda-mode-hook'.

Key bindings:
\\{cuda-mode-map}" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/cuda-mode-20201013.2230/cuda-mode" '("cuda-"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/cuda-mode-20201013.2230/cuda-mode-autoloads.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cuda-mode-autoloads.el ends here
