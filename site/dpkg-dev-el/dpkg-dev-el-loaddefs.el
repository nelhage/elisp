;;; dpkg-dev-el-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:

(provide 'dpkg-dev-el-loaddefs)

;;;### (autoloads (debian-bts-control) "debian-bts-control" "debian-bts-control.el"
;;;;;;  (18134 51684))
;;; Generated autoloads from debian-bts-control.el

(autoload (quote debian-bts-control) "debian-bts-control" "\
Contruct a message with initial ACTION command for control@bugs.debian.org.
Contructs a new control command line if called from within the message
being constructed.

If prefix arg is provided, use the current buffer instead instead of
creating a new outgoing email message buffer.
The current buffer is also used if the current major mode matches one listed
in `debian-bts-control-modes-to-reuse'." t nil)

;;;***

;;;### (autoloads (debian-changelog-mode debian-changelog-add-entry)
;;;;;;  "debian-changelog-mode" "debian-changelog-mode.el" (18432
;;;;;;  52589))
;;; Generated autoloads from debian-changelog-mode.el

(autoload (quote debian-changelog-add-entry) "debian-changelog-mode" "\
Add a new change entry to a debian-style changelog.
If called from buffer other than a debian/changelog, this will search
for the debian/changelog file to add the entry to." t nil)

(autoload (quote debian-changelog-mode) "debian-changelog-mode" "\
Major mode for editing Debian-style change logs.
Runs `debian-changelog-mode-hook' if it exists.

Key bindings:

\\{debian-changelog-mode-map}

If you want to use your debian.org email address for debian/changelog
entries without using it for the rest of your email, use the `customize`
interface to set it, or simply set the variable
`debian-changelog-mailing-address' in your ~/.emacs file, e.g.

 (setq debian-changelog-mailing-address \"myname@debian.org\"))" t nil)
(add-to-list 'auto-mode-alist '("/debian/*NEWS" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("NEWS.Debian" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("NEWS.Debian.gz" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("/debian/\\([[:lower:][:digit:]][[:lower:][:digit:].+-]+\\.\\)?changelog\\'" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("changelog.Debian" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("changelog.Debian.gz" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("changelog.dch" . debian-changelog-mode))

;;;***

;;;### (autoloads (debian-control-mode) "debian-control-mode" "debian-control-mode.el"
;;;;;;  (18410 57270))
;;; Generated autoloads from debian-control-mode.el

(autoload (quote debian-control-mode) "debian-control-mode" "\
A major mode for editing Debian control files (i.e. debian/control)." t nil)
(add-to-list 'auto-mode-alist '("/debian/control\\'" . debian-control-mode))

;;;***

;;;### (autoloads (debian-copyright-mode) "debian-copyright" "debian-copyright.el"
;;;;;;  (16295 49413))
;;; Generated autoloads from debian-copyright.el

(autoload (quote debian-copyright-mode) "debian-copyright" "\
Mode to edit and read debian/copyright.
\\{debian-copyright-mode-map}" t nil)
(add-to-list 'auto-mode-alist '("debian/.*copyright$" . debian-copyright-mode))
(add-to-list 'auto-mode-alist '("^/usr/share/doc/.*/copyright" . debian-copyright-mode))

;;;***

;;;### (autoloads (readme-debian-mode) "readme-debian" "readme-debian.el"
;;;;;;  (17503 21939))
;;; Generated autoloads from readme-debian.el

(autoload (quote readme-debian-mode) "readme-debian" "\
Mode for reading and editing README.Debian files.
Upon saving the visited README.Debian file, the timestamp at the bottom
will be updated.

\\{readme-debian-mode-map}" t nil)
(add-to-list 'auto-mode-alist '("debian/.*README.*Debian$" . readme-debian-mode))
(add-to-list 'auto-mode-alist '("^/usr/share/doc/.*/README.*Debian.*$" . readme-debian-mode))

;;;***

;;;### (autoloads (debian-bts-control) "debian-bts-control" "debian-bts-control.el"
;;;;;;  (16295 535))
;;; Generated autoloads from debian-bts-control.el

(autoload (quote debian-bts-control) "debian-bts-control" "\
Contruct a message with initial ACTION command for control@bugs.debian.org.
Contructs a new control command line if called from within the message
being constructed.

If prefix arg is provided, use the current buffer instead instead of
creating a new outgoing email message buffer.
The current buffer is also used if the current major mode matches one listed
in `debian-bts-control-modes-to-reuse'." t nil)

;;;***
