;;; svelte-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "svelte-mode" "../../../.emacs.d/elpa/svelte-mode-20210222.1037/svelte-mode.el"
;;;;;;  "7bbcb6b34925731967d263dc3e6537ca")
;;; Generated autoloads from ../../../.emacs.d/elpa/svelte-mode-20210222.1037/svelte-mode.el

(autoload 'svelte-mode "svelte-mode" "\
Major mode based on `html-mode', but works with embedded JS and CSS.

Code inside a <script> element is indented using the rules from
`js-mode'; and code inside a <style> element is indented using
the rules from `css-mode'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))

;;;### (autoloads "actual autoloads are elsewhere" "svelte-mode"
;;;;;;  "../../../.emacs.d/elpa/svelte-mode-20210222.1037/svelte-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/svelte-mode-20210222.1037/svelte-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "svelte-mode" '("svelte-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/svelte-mode-20210222.1037/svelte-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/svelte-mode-20210222.1037/svelte-mode.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; svelte-mode-autoloads.el ends here
