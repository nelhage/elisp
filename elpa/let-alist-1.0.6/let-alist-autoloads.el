;;; let-alist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "let-alist" "../../../.emacs.d/elpa/let-alist-1.0.6/let-alist.el"
;;;;;;  "a2959f38f2a4107eef9e49e54eb31c6f")
;;; Generated autoloads from ../../../.emacs.d/elpa/let-alist-1.0.6/let-alist.el

(autoload 'let-alist "let-alist" "\
Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
Dotted symbol is any symbol starting with a `.'.  Only those present
in BODY are let-bound and this search is done at compile time.

For instance, the following code

  (let-alist alist
    (if (and .title .body)
        .body
      .site
      .site.contents))

essentially expands to

  (let ((.title (cdr (assq \\='title alist)))
        (.body  (cdr (assq \\='body alist)))
        (.site  (cdr (assq \\='site alist)))
        (.site.contents (cdr (assq \\='contents (cdr (assq \\='site alist))))))
    (if (and .title .body)
        .body
      .site
      .site.contents))

If you nest `let-alist' invocations, the inner one can't access
the variables of the outer one. You can, however, access alists
inside the original alist by using dots inside the symbol, as
displayed in the example above.

\(fn ALIST &rest BODY)" nil t)

(function-put 'let-alist 'lisp-indent-function '1)

;;;### (autoloads "actual autoloads are elsewhere" "let-alist" "../../../.emacs.d/elpa/let-alist-1.0.6/let-alist.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/let-alist-1.0.6/let-alist.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "let-alist" '("let-alist--")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/let-alist-1.0.6/let-alist-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/let-alist-1.0.6/let-alist.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; let-alist-autoloads.el ends here
