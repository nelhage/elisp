;;; debian-el-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:

(provide 'debian-el-loaddefs)

;;;### (autoloads (apt-sources-mode) "apt-sources" "apt-sources.el"
;;;;;;  (16377 50278))
;;; Generated autoloads from apt-sources.el

(autoload (quote apt-sources-mode) "apt-sources" "\
Major mode for editing apt's sources.list file.
Sets up command `font-lock-mode'.

\\{apt-sources-mode-map}" t nil)

;;;***

;;;### (autoloads (apt-utils-search apt-utils-show-package) "apt-utils"
;;;;;;  "apt-utils.el" (17259 27466))
;;; Generated autoloads from apt-utils.el

(autoload (quote apt-utils-show-package) "apt-utils" "\
Show information for a Debian package.
A selection of known packages is presented.  See `apt-utils-mode'
for more detailed help.  If NEW-SESSION is non-nil, generate a
new `apt-utils-mode' buffer." t nil)

(autoload (quote apt-utils-search) "apt-utils" "\
Search Debian packages for regular expression.
To search for multiple patterns use a string like \"foo && bar\".
The regular expression used to split the
terms (`apt-utils-search-split-regexp') is customisable." t nil)

;;;***

;;;### (autoloads (deb-find deb-view-mode deb-view deb-view-dired-view)
;;;;;;  "deb-view" "deb-view.el" (17245 26632))
;;; Generated autoloads from deb-view.el

(autoload (quote deb-view-dired-view) "deb-view" "\
View Debian package control and data files.
Press \"q\" in either window to kill both buffers
and return to the dired buffer. See deb-view." t nil)

(autoload (quote deb-view) "deb-view" "\
View Debian package DEBFILE's control and data files.
Press \"q\" in either window to kill both buffers.

In dired, press ^d on the dired line of the .deb file to view.
Or, execute: ESCAPE x deb-view RETURN, and enter the .deb file name
at the prompt." t nil)

(autoload (quote deb-view-mode) "deb-view" "\
View mode for Debian Archive Files." t nil)

(autoload (quote deb-find) "deb-view" "\
Search for deb files.
Use the method specified by the variable deb-find-method, and collect
output in a buffer.  See also the variable deb-find-directory.

This command uses a special history list, so you can
easily repeat a `deb-find' command." t nil)

;;;***

;;;### (autoloads (debian-bug debian-bug-get-bug-as-email debian-bug-get-bug-as-file
;;;;;;  debian-bug-web-package debian-bug-web-packages debian-bug-web-this-bug-under-mouse
;;;;;;  debian-bug-web-bug debian-bug-web-developer-page debian-bug-web-bugs
;;;;;;  debian-bug-intent-to-package debian-bug-request-for-package
;;;;;;  debian-bug-wnpp) "debian-bug" "debian-bug.el" (17258 25899))
;;; Generated autoloads from debian-bug.el

(autoload (quote debian-bug-wnpp) "debian-bug" "\
Submit a WNPP bug report to Debian.
Optional argument ACTION can be provided in programs." t nil)

(autoload (quote debian-bug-request-for-package) "debian-bug" "\
Shortcut for `debian-bug-wnpp' with RFP action." t nil)

(autoload (quote debian-bug-intent-to-package) "debian-bug" "\
Shortcut for `debian-bug-wnpp' with ITP action (for Debian developers)." t nil)

(autoload (quote debian-bug-web-bugs) "debian-bug" "\
Browse the BTS for this package via `browse-url'.
With optional argument prefix ARCHIVED, display archived bugs." t nil)

(autoload (quote debian-bug-web-developer-page) "debian-bug" "\
Browse the web for this package's developer page." t nil)

(autoload (quote debian-bug-web-bug) "debian-bug" "\
Browse the BTS for BUG-NUMBER via `browse-url'." t nil)

(autoload (quote debian-bug-web-this-bug-under-mouse) "debian-bug" "\
Browse the BTS via `browse-url' for the bug report number under mouse.
In a program, mouse location is in EVENT." t nil)

(autoload (quote debian-bug-web-packages) "debian-bug" "\
Search Debian web page for this package via `browse-url'." t nil)

(autoload (quote debian-bug-web-package) "debian-bug" "\
Search Debian web page in ARCHIVE for this package via `browse-url'." t nil)

(autoload (quote debian-bug-get-bug-as-file) "debian-bug" "\
Read bug report #BUG-NUMBER as a regular file." t nil)

(autoload (quote debian-bug-get-bug-as-email) "debian-bug" "\
Read bug report #BUG-NUMBER via Email interface." t nil)

(autoload (quote debian-bug) "debian-bug" "\
Submit a Debian bug report." t nil)

;;;***

;;;### (autoloads (preseed-mode) "preseed" "preseed.el" (17245 35005))
;;; Generated autoloads from preseed.el

(autoload (quote preseed-mode) "preseed" "\
Major mode for editing debian-installer preseed files colourfully." t nil)

;;;***

;;;### (autoloads (apt-sources-mode) "apt-sources" "apt-sources.el"
;;;;;;  (16064 13921))
;;; Generated autoloads from apt-sources.el

(autoload (quote apt-sources-mode) "apt-sources" "\
Major mode for editing apt's sources.list file.
Sets up command `font-lock-mode'.

\\{apt-sources-mode-map}" t nil)

;;;***
