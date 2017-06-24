;;; debian-changelog-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "debian-changelog-mode" "debian-changelog-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from debian-changelog-mode.el

(autoload 'debian-changelog-add-entry "debian-changelog-mode" "\
Add a new change entry to a debian-style changelog.
If called from buffer other than a debian/changelog, this will search
for the debian/changelog file to add the entry to.

\(fn)" t nil)

(autoload 'debian-changelog-mode "debian-changelog-mode" "\
Major mode for editing Debian-style change logs.
Runs `debian-changelog-mode-hook' if it exists.

Key bindings:

\\{debian-changelog-mode-map}

If you want to use your debian.org email address for debian/changelog
entries without using it for the rest of your email, use the `customize`
interface to set it, or simply set the variable
`debian-changelog-mailing-address' in your ~/.emacs file, e.g.

 (setq debian-changelog-mailing-address \"myname@debian.org\"))

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("/debian/*NEWS" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("NEWS.Debian" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("/debian/\\([[:lower:][:digit:]][[:lower:][:digit:].+-]+\\.\\)?changelog\\'" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("changelog.Debian" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("changelog.dch" . debian-changelog-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "debian-changelog-mode" '("debian-changelog-" "imenu--create-debian-changelog-index")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; debian-changelog-mode-autoloads.el ends here
