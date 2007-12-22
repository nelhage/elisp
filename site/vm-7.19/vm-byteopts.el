;;(provide 'vm-byteopts)

;; get the compiler loaded so we can undo some of the things that
;; happen when it's loaded.
(load "bytecomp" t t nil)
;; Emacs 19 byte compiler complains about too much stuff by default.
;; Turn off most of the warnings here.
(setq byte-compile-warnings '(free-vars))
;; need to use these variables for v18 support.
;; stifle the compiler.
(put 'inhibit-local-variables 'byte-obsolete-variable nil)
;; Turn off dynamic docstrings and lazy function loading.  This
;; is a new feature of FSF Emacs 19.29, and is incompatible
;; with pre-19.29 versions of FSF Emacs and all version of Lucid
;; Emacs / XEmacs.  I like being able to share .elc files between
;; different v19 Emacses.
(setq byte-compile-dynamic nil)
(setq byte-compile-dynamic-docstrings nil)
;; avoid v20 features because users are going
;; to try to share elc files no matter what we tell them.
(setq byte-compile-emacs19-compatibility t)

(provide 'vm-byteopts)
