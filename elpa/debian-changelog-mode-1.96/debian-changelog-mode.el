;;; debian-changelog-mode.el --- major mode for Debian changelog files.

;;; Version: 1.96

;; Copyright (C) 1996 Ian Jackson
;; Copyright (C) 1997 Klee Dienes
;; Copyright (C) 1999 Chris Waters
;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 Peter S Galbraith
;; Copyright (C) 2006, 2007, 2009, 2010 Peter S Galbraith
;; Copyright (C) 2013 Pierre Carrier
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; debian-changelog-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your Debian installation, in /usr/share/common-licenses/GPL
;; If not, write to the Free Software Foundation, 51 Franklin Street,
;; Suite 500 Boston, MA 02110-1335, USA

;;; Commentary:
;;
;; This is a major mode for Debian changelog files.  The main features
;; are:
;;
;;  - fontification (varies with upload urgency, etc).
;;  - create a entry for a new version (guessing the version number).
;;  - finalize a version with proper timestamp and syntax.
;;  - add an entry from another file in the source package.
;;  - interface with `debian-bug' to fetch list of bugs from the web,
;;    read a bug report via browse-url or as email, close a bug with
;;    thanks.
;;  - closed bugs are fontified and clickable to view them via browse-url.
;;
;; The mode is entered automatically when editing a debian/changelog file.
;; See the menus "Bugs" and "Changelog" for commands or do `C-h m' to get
;; the list of keybindings.
;;
;; From other files in unpacked sources, do `M-x debian-changelog-add-entry'
;; to add an entry for that file in the changelog file.

;;; Acknowledgements:  (These people have contributed)
;;   Roland Rosenfeld <roland@debian.org>
;;   James LewisMoss <dres@ioa.com>
;;   Rafael Laboissiere <rafael@icp.inpg.fr>
;;   Brian Warner <warner@lothar.com>
;;   Yann Dirson <dirson@debian.org>

;;; Code:

(defgroup debian-changelog nil "Debian changelog maintenance"
  :group 'tools
  :prefix "debian-changelog-")

(defgroup debian-changelog-faces nil
  "Faces for fontifying text in debian-changelog."
  :prefix "debian-changelog-"
  :group 'debian-changelog)

(defcustom debian-changelog-full-name (or (getenv "DEBFULLNAME")
                                          (user-full-name))
  "*Full name of user, for inclusion in Debian changelog headers.
This defaults to the contents of environment variable DEBFULLNAME
or else to the value returned by the function `user-full-name'."
  :group 'debian-changelog
  :type 'string)

(defcustom debian-changelog-mailing-address
  (or (getenv "DEBEMAIL")
      (getenv "EMAIL")
      (and (boundp 'user-mail-address) user-mail-address)
      (and (fboundp 'user-mail-address) (user-mail-address)))
  "*Electronic mail address of user, for inclusion in Debian changelog headers.
This defaults to the value of (in order of precedence):
 Contents of environment variable DEBEMAIL,
 Contents of environment variable EMAIL,
 Value of `user-mail-address' variable,
 Value returned by the `user-mail-address' function."
  :group 'debian-changelog
  :type 'string)

(defcustom debian-changelog-local-variables-maybe-remove t
  "*Ask to remove obsolete \"Local Variables:\" block from changelog.
This is done only under certain conditions."
  :group 'debian-changelog
  :type 'boolean)

(defcustom debian-changelog-highlight-mouse-t t
  "*Use special overlay for bug numbers, defining mouse-3 to web interface."
  :group 'debian-changelog
  :type 'boolean)

(defcustom debian-changelog-use-imenu (fboundp 'imenu-add-to-menubar)
  "*Use imenu package for debian-changelog-mode?
If you do not wish this behaviour, reset it in your .emacs file like so:

  (setq debian-changelog-use-imenu nil)"
  :group 'debian-changelog
  :type 'boolean)

;; This solves the consistency problem with `debian-changelog-close-bug'
;; as per bug #431091
(defcustom debian-changelog-close-bug-statement "(Closes: #%s)."
  "The text to be inserted to close a bug.  `%s' is replaced by
the bug number."
  :group 'debian-changelog
  :type 'string)

(defcustom debian-changelog-mode-hook nil
  "Normal hook run when entering Debian Changelog mode."
  :group 'debian-changelog
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode))

(defcustom debian-changelog-add-version-hook
  (list 'debian-changelog-add-new-upstream-release)
  "Hooks run just before inserting the signature separator \"--\" in a
new version in debian/changelog."
  :group 'debian-changelog
  :type 'hook)

(defcustom debian-changelog-date-utc-flag nil
  "If non-nil, return date string in UTC when finalizing entry.
See function `debian-changelog-date-string'."
  :group 'debian-changelog
  :type 'boolean)

;; This function is from emacs/lisp/calendar/icalendar.el,
;; necessary to replace "%s" with the bug number in
;; `debian-changelog-close-bug-statement'
(defsubst debian-changelog--rris (&rest args)
  "Replace regular expression in string.
Pass ARGS to `replace-regexp-in-string' (GNU Emacs) or to
`replace-in-string' (XEmacs)."
  ;; XEmacs:
  (if (fboundp 'replace-in-string)
      (save-match-data ;; apparently XEmacs needs save-match-data
        ;; and arguments are in different order.
        ;; Patch from Rafael Laboissiere <rafael@debian.org>
        ;; Closes: #476271
        (apply 'replace-in-string (list (nth 2 args) (nth 0 args) (nth 1 args))))
    ;; Emacs:
    (apply 'replace-regexp-in-string args)))

(defvar debian-changelog-local-variables-maybe-remove-done nil
  "Internal flag so we prompt only once.")

(autoload 'debian-bug-web-bug "debian-bug")
(autoload 'debian-bug-web-bugs "debian-bug")
(autoload 'debian-bug-web-packages "debian-bug")
(autoload 'debian-bug-web-package "debian-bug")
(autoload 'debian-bug-bug-menu-init "debian-bug")
(autoload 'debian-bug-web-this-bug-under-mouse "debian-bug")
(autoload 'debian-bug-web-developer-page "debian-bug")
(defvar debian-bug-open-alist)


(require 'add-log)
(require 'easymenu)
(eval-when-compile
  (require 'cl))

;; XEmacs21.1 compatibility -- from XEmacs's apel/poe.el
(unless (fboundp 'match-string-no-properties)
  (defun match-string-no-properties (num &optional string)
    "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
    (if (match-beginning num)
        (if string
            (let ((result
                   (substring string (match-beginning num) (match-end num))))
              (set-text-properties 0 (length result) nil result)
              result)
          (buffer-substring-no-properties (match-beginning num)
                                          (match-end num))))))
;;
;; Clean up old "Local Variables:" entries
;; Peter Galbraith

;; **Important note**
;;
;;  If we get the following warning:
;;
;;   File local-variables error: (error "Local variables entry is missing the prefix")
;;
;;  when installing the dpkg-dev-el package, it's because the command
;;  (hack-local-variables) from files.el is bailing on all the "Local
;;  Variables:" strings in this file.  The simplest solution is to keep all
;;  occurrences of this string before the last 3000 characters of the file,
;;  where `hack-local-variables' starts looking:

;; First, I made the add-log-mailing-address variable obsolete but still
;; left the "mode:" line in the variable block for Debian native packages
;; because it was impossible to tell what they were from the installed
;; changelog.gz name.  In bug #105889, I came up with code to stick in
;; /etc/emacs/site-start.d/50dpkg-dev-el.el to figure that out in a
;; find-file-hooks hook.  So now the variable block is completely obsolete.
(defun debian-changelog-local-variables-maybe-remove ()
  "Ask to remove local variables block if buffer not read-only."
  (interactive)
  (if (or debian-changelog-local-variables-maybe-remove-done
          buffer-read-only)
      nil
    (setq debian-changelog-local-variables-maybe-remove-done t)
    (if (debian-changelog-local-variables-exists-p)
        (save-excursion
          (goto-char (point-max)) ; local vars are always at end
          (if (yes-or-no-p
               "Remove obsolete \"local variables:\" from changelog? ")
              (debian-changelog-local-variables-remove))))))

(defun debian-changelog-local-variables-exists-p ()
  "Return t if package has a \"Local Variables:\" block."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-max))
      (and (re-search-backward "^local variables:" nil t)
           (or (re-search-forward "add-log-mailing-address:" nil t)
               (re-search-forward "mode: debian-changelog" nil t))))))

(defun debian-changelog-local-variables-remove ()
  "Remove `add-log-mailing-address' entry from local variables block."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-max))
      ;; Remove add-log-mailing-address: line if it exists
      (if (and (re-search-backward "^local variables:" nil t)
               (re-search-forward "add-log-mailing-address: .+\n" nil t))
          (delete-region (match-beginning 0)(match-end 0)))
      (goto-char (point-max))
      ;; Remove "mode: debian-changelog" line if it exists
      (if (and (re-search-backward "^local variables:" nil t)
               (re-search-forward "mode: debian-changelog.*\n" nil t))
          (delete-region (match-beginning 0)(match-end 0)))
      (goto-char (point-max))
      ;; Remove empty variable block if it exists
      (if (re-search-backward "^local variables: *\nend:" nil t)
          (delete-region (match-beginning 0)(match-end 0))))))

;;
;; internal functions: getheadervalue and setheadervalue both use a
;; regexp to probe the changelog entry for specific fields.

;; warning: if used with a "re" that doesn't have at least one group,
;; the results will be unpredictable (to say the least).

(defun debian-changelog-setheadervalue (re str)
  (if (eq (debian-changelog-finalised-p) t)
      (error (substitute-command-keys "most recent version has been finalised - use \\[debian-changelog-unfinalise-last-version] or \\[debian-changelog-add-version]")))
  (let ((lineend (save-excursion (end-of-line)(point))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward re lineend t)
          (let ((a (match-beginning 1))
                (b (match-end 1)))
            (goto-char a)
            (delete-region a b)
            (insert str))))))

(defun debian-changelog-getheadervalue (re)
  (let ((lineend (save-excursion (end-of-line) (point))))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward re lineend)
      (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))

;;
;; some get/set functions for specific fields
;;  (Chris Waters)

(defun debian-changelog-seturgency (val)
  (debian-changelog-setheadervalue "\\;[^\n]* urgency=\\(\\sw+\\)" val))
(defun debian-changelog-geturgency ()
  (debian-changelog-getheadervalue "\\;[^\n]* urgency=\\(\\sw+\\)"))
(defun debian-changelog-getdistribution ()
  (debian-changelog-getheadervalue ") \\(.*\\)\\;"))
(defvar last-nonmenu-event)
(defun debian-changelog-setdistribution (val)
  (if (not (string-match "^.*security" val))
      (debian-changelog-setheadervalue ") \\(.*\\)\\;" val)
    (cond
     ((or (and (fboundp 'should-use-dialog-box-p)
               (should-use-dialog-box-p))
          (and window-system
               (equal last-nonmenu-event '(menu-bar))
               use-dialog-box))
      (if (y-or-n-p
           (concat
            "Warning, although the {oldstable,stable,testing}-security
distribution exists it should not be used unless you are a
member of the security team.  Please don't upload to it if you
are not 150% sure that your package is suitable.  In case of
doubt, please send the files to team@security.debian.org via
mail instead.

Upload to " val  " anyway?"))
          (debian-changelog-setheadervalue ") \\(.*\\)\\;" val)))
     (t
      (let ((window-config (current-window-configuration)))
        (with-output-to-temp-buffer "*Help*"
          (princ (concat
                  "Warning, although the {oldstable,stable,testing}-security
distribution exists it should not be used unless you are a
member of the security team.  Please don't upload to it if you
are not 150% sure that your package is suitable.  In case of
doubt, please send the files to team@security.debian.org via
mail instead.

Upload to " val  " anyway?")))
        (if (y-or-n-p (format "Upload to %s anyway? " val))
            (debian-changelog-setheadervalue ") \\(.*\\)\\;" val))
        (set-window-configuration window-config))))))

;;
;; keymap table definition
;;

(autoload 'outline-next-visible-heading "outline")
(autoload 'outline-prev-visible-heading "outline")

(defvar debian-changelog-mode-map nil
  "Keymap for Debian changelog major mode.")
(if debian-changelog-mode-map
    nil
  (setq debian-changelog-mode-map (make-sparse-keymap))
  (define-key debian-changelog-mode-map "\C-c\C-a"
    'debian-changelog-add-entry)
  (define-key debian-changelog-mode-map "\C-c\C-o"
    'debian-changelog-build-open-bug-list)
  (define-key debian-changelog-mode-map "\C-c\C-b"
    'debian-changelog-close-bug)
  (define-key debian-changelog-mode-map "\C-c\C-f"
    'debian-changelog-finalise-last-version)
  (define-key debian-changelog-mode-map "\C-c\C-c"
    'debian-changelog-finalise-and-save)
  (define-key debian-changelog-mode-map "\C-c\C-v"
    'debian-changelog-add-version)
  (define-key debian-changelog-mode-map "\C-c\C-d"
    'debian-changelog-distribution)
  (define-key debian-changelog-mode-map "\C-c\C-u"
    'debian-changelog-urgency)
  (define-key debian-changelog-mode-map "\C-c\C-e"
    'debian-changelog-unfinalise-last-version)
  (define-key debian-changelog-mode-map "\C-c\C-n"
    'outline-next-visible-heading)
  (define-key debian-changelog-mode-map "\C-c\C-p"
    'outline-previous-visible-heading))


;;
;; menu definition (Chris Waters)
;;

(defvar debian-changelog-is-XEmacs
  (and
   (not (null (save-match-data (string-match "XEmacs\\|Lucid" emacs-version))))
   (= 21 emacs-major-version)))

(cond
 (debian-changelog-is-XEmacs
  (easy-menu-define
    debian-changelog-menu debian-changelog-mode-map "Debian Changelog Mode Menu"
    '("Changelog"
      ["New Version" debian-changelog-add-version (debian-changelog-finalised-p)]
      ["Add Entry" debian-changelog-add-entry
       (not (debian-changelog-finalised-p))]
      ["Build Open Bug List" debian-changelog-build-open-bug-list]
      ["Close Bug" debian-changelog-close-bug
       (not (debian-changelog-finalised-p))]
      "--"
      ("Set Distribution"
       ["unstable" (debian-changelog-setdistribution "unstable") t]
       ("--")
       ["testing" (debian-changelog-setdistribution "testing") t]
       ["testing-security" (debian-changelog-setdistribution "testing-security") t]
       ("--")
       ["stable" (debian-changelog-setdistribution "stable") t]
       ["stable-security" (debian-changelog-setdistribution "stable-security") t]
       ["oldstable-security" (debian-changelog-setdistribution "oldstable-security") t]
       ("--")
       ["experimental" (debian-changelog-setdistribution "experimental") t]
       ["UNRELEASED" (debian-changelog-setdistribution "UNRELEASED") t])
      ("Set Urgency"
       ["low" (debian-changelog-seturgency "low") t]
       ["medium" (debian-changelog-seturgency "medium") t]
       ["high" (debian-changelog-seturgency "high") t]
       ["critical" (debian-changelog-seturgency "critical") t])
      "--"
      ["Unfinalise" debian-changelog-unfinalise-last-version
       (debian-changelog-finalised-p)]
      ["Finalise" debian-changelog-finalise-last-version
       (not (debian-changelog-finalised-p))]
      ["Finalise+Save" debian-changelog-finalise-and-save
       (not (debian-changelog-finalised-p))]
      "--"
      "Web View"
      ["Best Practices" (browse-url "http://www.debian.org/doc/developers-reference/best-pkging-practices.html#bpp-debian-changelog") t]
      ["Bugs for This Package" (debian-bug-web-bugs) t]
      ["Archived Bugs for This Package" (debian-bug-web-bugs t) t]
      ["Bug Number..." (debian-bug-web-bug) t]
      ["Package Info" (debian-bug-web-packages) t]
      ;; ("Package web pages..."
      ;;  ["stable" (debian-bug-web-package "stable") t]
      ;;  ["testing" (debian-bug-web-package "testing") t]
      ;;  ["unstable" (debian-bug-web-package "unstable") t])
      ["Developer Page for This Package" (debian-bug-web-developer-page) t]
      ["Developer Page for This Maintainer" (debian-changelog-web-developer-page)
       t]
      "--"
      ["Customize" (customize-group "debian-changelog") (fboundp 'customize-group)])))
 (t
  (easy-menu-define
    debian-changelog-menu debian-changelog-mode-map "Debian Changelog Mode Menu"
    '("Changelog"
      ["New Version" debian-changelog-add-version (debian-changelog-finalised-p)]
      ["Add Entry" debian-changelog-add-entry
       (not (debian-changelog-finalised-p))]
      ["Build Open Bug List" debian-changelog-build-open-bug-list]
      ["Close Bug" debian-changelog-close-bug
       (not (debian-changelog-finalised-p))]
      "--"
      ("Set Distribution"     :active (not (debian-changelog-finalised-p))
       ["unstable" (debian-changelog-setdistribution "unstable") t]
       ("--")
       ["testing" (debian-changelog-setdistribution "testing") t]
       ["testing-security" (debian-changelog-setdistribution "testing-security") t]
       ("--")
       ["stable" (debian-changelog-setdistribution "stable") t]
       ["stable-security" (debian-changelog-setdistribution "stable-security") t]
       ["oldstable-security" (debian-changelog-setdistribution "oldstable-security") t]
       ("--")
       ["experimental" (debian-changelog-setdistribution "experimental") t]
       ["UNRELEASED" (debian-changelog-setdistribution "UNRELEASED") t])
      ("Set Urgency"     :active (not (debian-changelog-finalised-p))
       ["low" (debian-changelog-seturgency "low") t]
       ["medium" (debian-changelog-seturgency "medium") t]
       ["high" (debian-changelog-seturgency "high") t]
       ["critical" (debian-changelog-seturgency "critical") t])
      "--"
      ["Unfinalise" debian-changelog-unfinalise-last-version
       (debian-changelog-finalised-p)]
      ["Finalise" debian-changelog-finalise-last-version
       (not (debian-changelog-finalised-p))]
      ["Finalise+Save" debian-changelog-finalise-and-save
       (not (debian-changelog-finalised-p))]
      "--"
      "Web View"
      ["Best Practices" (browse-url "http://www.debian.org/doc/developers-reference/ch-best-pkging-practices.en.html#s-bpp-debian-changelog") t]
      ["Bugs for This Package" (debian-bug-web-bugs) t]
      ["Archived Bugs for This Package" (debian-bug-web-bugs t) t]
      ["Bug Number..." (debian-bug-web-bug) t]
      ["Package Info" (debian-bug-web-packages) t]
      ("Package web pages..."
       ["stable" (debian-bug-web-package "stable") t]
       ["testing" (debian-bug-web-package "testing") t]
       ["unstable" (debian-bug-web-package "unstable") t])
      ["Developer Page for This Package" (debian-bug-web-developer-page) t]
      ["Developer Page for This Maintainer" (debian-changelog-web-developer-page)
       t]
      "--"
      ["Customize" (customize-group "debian-changelog") (fboundp 'customize-group)]))))

;;
;; interactive function to add a new line to the changelog
;;

;;;###autoload
(defun debian-changelog-add-entry ()
  "Add a new change entry to a debian-style changelog.
If called from buffer other than a debian/changelog, this will search
for the debian/changelog file to add the entry to."
  (interactive)
  (if (string-match ".*/debian/changelog" (buffer-file-name))
      (debian-changelog-add-entry-plain)
    (debian-changelog-add-entry-file)))

(defun debian-changelog-add-entry-plain ()
  "Add a new change entry to a debian-style changelog."
  (if (eq (debian-changelog-finalised-p) t)
      (error (substitute-command-keys "most recent version has been finalised - use \\[debian-changelog-unfinalise-last-version] or \\[debian-changelog-add-version]")))
  (goto-char (point-min))
  (re-search-forward "\n --")
  (backward-char 5)
  (if (prog1 (looking-at "\n") (forward-char 1))
      nil
    (insert "\n"))
  (insert "  * ")
  (save-excursion (insert "\n")))

(defun debian-changelog-add-entry-file ()
  "Add an entry for current file in debian/changelog."
  (let* ((this-file (buffer-file-name))
         (directory (if (not this-file)
                        (error "This buffer has no file associated to it")
                      (directory-file-name (file-name-directory this-file))))
         (filename (file-name-nondirectory this-file))
         (success))
    (while directory
      (let ((changelog (expand-file-name "debian/changelog" directory)))
        (cond
         ((file-readable-p changelog)
          (debian-changelog-add-entry-file-specified changelog filename)
          (setq directory nil
                success t))
         (t
          (if (not (string-match "\\(.*\\)/\\([^/]+\\)$" directory))
              (setq directory nil)
            (setq filename (concat (match-string 2 directory) "/" filename)
                  directory (match-string 1 directory)))))))
    (if (not success)
        (error "debian directory not found"))))

(defun debian-changelog-add-entry-file-specified (changelog filename)
  "Insert an entry in debian CHANGELOG file for FILENAME."
  (interactive)
  (find-file changelog)
  (if (eq (debian-changelog-finalised-p) t)
      (let ((action (capitalize
                     (read-string
                      "Most recent version is finalised, [u]nfinalize or [a]dd new version? "))))
        (if (not (string-match "^[uU]" action))
            (debian-changelog-add-version)
          (debian-changelog-unfinalise-last-version)
          (debian-changelog-add-entry-plain)))
    (debian-changelog-add-entry-plain))
  (insert filename ": "))

;;
;; interactive function to close bugs by number. (Peter Galbraith)
;;

(defvar debian-changelog-close-bug-takes-arg t
  "A compatibility flag for debian-bug.el.")

(defun debian-changelog-build-open-bug-list ()
  "Generate open bugs list, i.e. `debian-bug-open-alist'."
  (interactive)
  (debian-bug-build-bug-menu (debian-changelog-suggest-package-name) t))

(defun debian-changelog-close-bug (bug-number)
  "Add a new change entry to close a BUG-NUMBER."
  (interactive
   (progn
     (if (eq (debian-changelog-finalised-p) t)
         (error (substitute-command-keys "most recent version has been finalised - use \\[debian-changelog-unfinalise-last-version] or \\[debian-changelog-add-version]")))
     (list (completing-read "Bug number to close: "
                            debian-bug-open-alist nil nil))))
  (if (not (string-match "^[0-9]+$" bug-number))
      (error "The bug number should consists of only digits"))
  (debian-changelog-add-entry)
  (cond
   ((and debian-bug-open-alist
         (assoc bug-number debian-bug-open-alist))
    (insert (cadr (assoc bug-number debian-bug-open-alist)))
    (fill-paragraph nil))
   (t
    (save-excursion
      (insert " " (debian-changelog--rris
                   "%s" bug-number debian-changelog-close-bug-statement)))
    (message "Enter a brief description of what was done here."))))

;;
;; interactive functions to set urgency and distribution
;;

(defun debian-changelog-distribution ()
  "Delete the current distribution and prompt for a new one."
  (interactive)
  (if (eq (debian-changelog-finalised-p) t)
      (error (substitute-command-keys "most recent version has been finalised - use \\[debian-changelog-unfinalise-last-version] or \\[debian-changelog-add-version]")))
  (let ((str (completing-read
              "Select distribution: "
              '(("unstable" 1)
                ("testing" 2)
                ("testing-security" 3)
                ("stable" 4)
                ("stable-security" 5)
                ("oldstable-security" 6)
                ("experimental" 7)
                ("UNRELEASED" 8))
              nil t nil)))
    (if (not (equal str ""))
        (debian-changelog-setdistribution str))))

(defun debian-changelog-urgency ()
  "Delete the current urgency and prompt for a new one."
  (interactive)
  (if (eq (debian-changelog-finalised-p) t)
      (error (substitute-command-keys "most recent version has been finalised - use \\[debian-changelog-unfinalise-last-version] or \\[debian-changelog-add-version]")))
  (let ((str (completing-read
              "Select urgency: "
              '(("low" 1) ("medium" 2) ("high" 3) ("critical" 4))
              nil t nil)))
    (if (not (equal str ""))
        (debian-changelog-seturgency str))))

;;
;; internal function: test if changelog has been finalized or not
;; New version by Tommi Virtanen <tv@debian.org>
;; Sun, 24 Jun 2001 16:03:01 UTC;  Debian bug #102088
;; -
;; regexp tweaked by psg, Tue Jul 10 15:29:54 EDT 2001

(defun debian-changelog-finalised-p ()
  "Check whether the most recent debian-style changelog entry is finalised yet.
\(ie, has a maintainer name and email address and a release date."
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward "\n\\S-" (point-max) t)
        (goto-char (point-max)))
    (if (re-search-backward "\n --" (point-min) t)
        (forward-char 4)
      ;;(beginning-of-line)
      ;;(insert " --\n\n")
      ;;(backward-char 2)
      )
    (cond
     ((looking-at
       "[ \n]+\\S-[^\n\t]+\\S- <[^ \t\n<>]+> +\\S-[^\t\n]+\\S-[ \t]*\n")
      t)
     ((looking-at "[ \t]*\n")
      nil)
     (t
      "finalisation line has bad format (not ` -- maintainer <email> date')"))))
;;
;;  interactive functions to add new versions (whole new sections)
;;  to changelog.
;;

(defvar debian-changelog-new-upstream-release-p nil)

(defun debian-changelog-add-new-upstream-release ()
  "Normal hook for adding \"new upstream release\" entry to changelog."
  (when debian-changelog-new-upstream-release-p
    (insert "New upstream release")
    (setq debian-changelog-new-upstream-release-p nil)))

(defun debian-changelog-add-version ()
  "Add a new version section to a debian-style changelog file.
If file is empty, create initial entry."
  (interactive)
  (if (not (= (point-min)(point-max)))
      (let ((f (debian-changelog-finalised-p)))
        (and (stringp f) (error f))
        (or f (error "Previous version not yet finalised"))))
  (goto-char (point-min))
  (let ((pkg-name (or (debian-changelog-suggest-package-name)
                      (read-string "Package name: ")))
        (version (or (debian-changelog-suggest-version)
                     (read-string "New version (including any revision): "))))
    (if (debian-changelog-experimental-p)
        (insert pkg-name " (" version ") experimental; urgency=low\n\n  * ")
      (insert pkg-name " (" version ") unstable; urgency=low\n\n  * "))
    (run-hooks 'debian-changelog-add-version-hook)
    (save-excursion (insert "\n\n --\n\n"))))

(defun debian-changelog-experimental-p ()
  ;; Peter S Galbraith, 04 May 2001
  "Return t if last upload is to experimental."
  (save-excursion
    (goto-char (point-min))
    (looking-at "\\sw.* (.+).* \\(experimental\\)")))

(defun debian-changelog-suggest-package-name ()
  ;; Peter S Galbraith, 23 Feb 2001
  "Return package name from first line of the changelog, or nil."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at
         "\\(\\S-+\\) +(\\([^()\n\t-]+\\)\\(-\\([^()]+\\)\\)?\\() +[^\n]*\\)")
        (match-string-no-properties 1))))

(defun debian-changelog-greater-than (vsn1 vsn2)
  "Return t if VSN1 is greater than VSN2."
  (save-excursion
    (let ((tmp-buffer (get-buffer-create
                       " *debian-changelog-mode-temp-buffer*")))
      (set-buffer tmp-buffer)
      (unwind-protect
          (progn
            (let ((mesg (call-process "dpkg" nil '(t nil) nil
                                      "--compare-versions" vsn1 "gt" vsn2)))
              (if (equal mesg 0)
                  t
                nil)))
        (kill-buffer tmp-buffer)))))

(defun debian-changelog-suggest-version ()
  ;; Peter S Galbraith, 23 Feb 2001
  "Return a suggested new version number to use for this changelog, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((findmatch t))
      (cond
       ((looking-at
;;; The following is not strictly correct.  The upstream version may actually
;;; contain a hyphen if a debian version number also exists, making two hyphens
;;; I'm also assuming it begins with a digit, which is not enforced
         "\\(\\S-+\\) +(\\([0-9]:\\)?\\([0-9][0-9a-zA-Z.+:~]*\\)\\(-\\([0-9a-zA-Z.+~]+\\)\\)?\\() +[^\n]*\\)"))

       ;; No match...
       ;; Check again for multiple hyphens, and adjust match-data if found
       ;; to leave only the bit past the last hyphen as the debian version
       ;; number.
       ((looking-at
         "\\(\\S-+\\) +(\\([0-9]:\\)?\\([0-9][0-9a-zA-Z.+:~]*\\)\\(-\\([0-9a-zA-Z.+~]+\\)\\)*\\() +[^\n]*\\)")
        ;; We have a hit.  Adjust match-data...
        (goto-char (match-end 5))
        (skip-chars-backward "0-9a-zA-Z.+~")
        (let ((deb-vsn-beg (point))
              (ups-vsn-end (1- (point))))
          (store-match-data
           (list
            (match-beginning 0)(match-end 0)
            (match-beginning 1)(match-end 1)
            (match-beginning 2)(match-end 2)
            (match-beginning 3) ups-vsn-end
            (match-beginning 4)(match-end 4)
            deb-vsn-beg        (match-end 5)
            (match-beginning 6)(match-end 6)))))
       (t
        (setq findmatch nil)))


;;; match 1: package name
;;; match 2: epoch, if it exists
;;; match 3: upstream version number
;;; match 4: debian version number exists if matched
;;; match 5: debian version number
;;; match 6: rest of string
      (if (not findmatch)
          nil
        (let ((pkg-name (match-string-no-properties 1))
              (epoch (or (match-string-no-properties 2) ""))
              (upstream-vsn (match-string-no-properties 3))
              (debian-vsn (match-string-no-properties 5)))
          ;;debug (message "name: %s  epoch: %s  version: %s  debian: %s" pkg-name epoch upstream-vsn debian-vsn))))

          (cond
           ;; Debian vsn exists + Old upstream version matches current one.
           ;; -> Increment Debian version...
           ((and debian-vsn
                 (string-match
                  (regexp-quote (concat "/" pkg-name "-" upstream-vsn "/debian/changelog"))
                  buffer-file-name))
            (concat epoch upstream-vsn "-"
                    (debian-changelog-increment-version debian-vsn)))

           ;; Same as above, but more general in case directory name doesn't
           ;; match package name.  -> Increment Debian version...
           ((and debian-vsn
                 (string-match
                  (concat "-" (regexp-quote upstream-vsn) "/debian/changelog")
                  buffer-file-name))
            (concat epoch upstream-vsn "-"
                    (debian-changelog-increment-version debian-vsn)))

           ;; Debian vsn exists but old upstream version doesn't match new one.
           ;; -> Use new upstream version with "-1" debian version.
;;;FIXME: I should perhaps check that the directory name version is higher
;;;than that currently in changelog.
           ((and debian-vsn
                 (string-match (concat
                                "/"
                                (regexp-quote pkg-name)
                                "-\\([0-9][0-9a-zA-Z.+~-]+\\)/debian/changelog")
                               buffer-file-name))
            (setq debian-changelog-new-upstream-release-p t)
            (concat epoch (match-string 1 buffer-file-name) "-1"))

           ;; Same as above, but more general in case directory name doesn't
           ;; match package name.
           ;; -> Use new upstream version with "-1" debian version.
           ((and debian-vsn
                 (string-match
                  (concat "-\\([0-9][0-9a-zA-Z.+~-]+\\)/debian/changelog")
                  buffer-file-name))
            (setq debian-changelog-new-upstream-release-p t)
            (concat epoch (match-string 1 buffer-file-name) "-1"))

           ;; Debian vsn exists, but directory name has no version
           ;; -> increment Debian vsn (no better guess)
           (debian-vsn
            (concat epoch upstream-vsn "-"
                    (debian-changelog-increment-version debian-vsn)))

         ;;; No Debian version number...

           ;; No debian version number and version number from changelog
           ;; already greater than from directory name.
           ((and (not debian-vsn)
                 (not (string-match
                       (concat "/" (regexp-quote pkg-name) "-"
                               (regexp-quote upstream-vsn) "/debian/changelog")
                       buffer-file-name))
                 (string-match (concat "/" (regexp-quote pkg-name)
                                       "-\\([0-9a-zA-Z.+~]+\\)/debian/changelog")
                               buffer-file-name)
                 (debian-changelog-greater-than
                  upstream-vsn (match-string 1 buffer-file-name)))
            (concat epoch (debian-changelog-increment-version upstream-vsn)))

           ;; No debian version number (Debian native) and old upstream
           ;; version matches new one (e.g. 'dpk-source -x package' without
           ;; then bumping up the version in the directory name.
           ((and (not debian-vsn)
                 (string-match (concat "/" (regexp-quote pkg-name) "-"
                                       (regexp-quote upstream-vsn)
                                       "/debian/changelog")
                               buffer-file-name)
                 (concat epoch
                         (debian-changelog-increment-version upstream-vsn))))

           ;; No debian version number and version number from changelog
           ;; less than from directory name.
           ((and (not debian-vsn)
                 (not (string-match
                       (concat "/" (regexp-quote pkg-name) "-"
                               (regexp-quote upstream-vsn) "/debian/changelog")
                       buffer-file-name))
                 (string-match (concat
                                "/" (regexp-quote pkg-name)
                                "-\\([0-9a-zA-Z.+~]+\\)/debian/changelog")
                               buffer-file-name)
                 (debian-changelog-greater-than
                  (match-string 1 buffer-file-name) upstream-vsn))
            (concat epoch (match-string 1 buffer-file-name)))

           ((string-match (concat "/" (regexp-quote pkg-name)
                                  "-\\([0-9a-zA-Z.+~]+\\)/debian/changelog")
                          buffer-file-name)
            ;;Hmmm.. return version number from directory if we get this far
            (concat epoch (match-string 1 buffer-file-name)))
           ((string-match
             (concat "-\\([0-9][0-9a-zA-Z.+~]+\\)/debian/changelog")
             buffer-file-name)
            ;;Hmmm.. return version number from directory if we get this far
            (concat epoch (match-string 1 buffer-file-name)))

           ;; Directory name has no version -> increment what we have.
           (t
            (concat epoch
                    (debian-changelog-increment-version upstream-vsn)))))))))

(defun debian-changelog-increment-version (version)
  ;; Peter S Galbraith, 09 Mar 2001
  "Increment the last numeric portion of a VERSION number.
1        -> 2
0potato1 -> 0potato2
1.01     -> 1.02"
  (cond
   ((string-match "[1-9][0-9]*$" version)
    (let ((first-part (substring version 0 (match-beginning 0)))
          (snd-part (match-string 0 version)))
      (concat
       first-part (number-to-string (+ 1 (string-to-number snd-part))))))
   ((string-match "[0-9]*$" version)
    ;; 3.5.4.0 -> 3.5.4.1
    (let ((first-part (substring version 0 (match-beginning 0)))
          (snd-part (match-string 0 version)))
      (concat
       first-part (number-to-string (+ 1 (string-to-number snd-part))))))
   (t
    ;; Safety net only - first condition should catch all
    (number-to-string (+ 1 (string-to-number version))))))

(defun debian-changelog-finalise-and-save ()
  "Finalise, if necessary, and then save a debian-style changelog file."
  (interactive)
  (let ((f (debian-changelog-finalised-p)))
    (and (stringp f) (error f))
    (or f (debian-changelog-finalise-last-version)))
  (save-buffer))

;;
;; internal function to get date as string (used by finalising routines)
;;

(defun debian-changelog-date-string ()
  "Return RFC-822 format date string.
Use UTC if `debian-changelog-date-utc-flag' is non-nil."
  (let ((system-time-locale "C"))
    (format-time-string "%a, %d %b %Y %T %z" nil
                        debian-changelog-date-utc-flag)))

;;
;; interactive functions to finalize entry
;;

;;; Use debian-changelog-full-name and debian-changelog-mailing-address instead
;; (make-local-variable 'add-log-full-name)
;; (make-local-variable 'add-log-mailing-address)

(defun debian-changelog-finalise-last-version ()
  "Finalise maintainer's name and email and release date."
  (interactive)
  (and (debian-changelog-finalised-p)
       (debian-changelog-unfinalise-last-version))
  (if debian-changelog-local-variables-maybe-remove
      (debian-changelog-local-variables-maybe-remove))
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\n --\\([ \t]*\\)")
    (delete-region (match-beginning 1) (match-end 1))
    (insert " " debian-changelog-full-name
            " <" debian-changelog-mailing-address ">  "
            (debian-changelog-date-string))))

(defun debian-changelog-last-maintainer ()
  "Return maintainer name and e-mail of the last changelog entry as
a list in the form (NAME EMAIL)."
  (save-excursion
    (goto-char (point-min))
    (let ((string
           (if (re-search-forward "^ -- \\(.*\\)>" nil t)
               (if (fboundp 'match-string-no-properties)
                   (match-string-no-properties 1)
                 (match-string 1))
             (error "Maintainer name and email not found."))))
      (split-string string " <"))))

(defun debian-changelog-web-developer-page ()
  "Browse the BTS for the last upload maintainer's developer summary page."
  (interactive)
  (if (not (featurep 'browse-url))
      (progn
        (load "browse-url" nil t)
        (if (not (featurep 'browse-url))
            (error "This function requires the browse-url elisp package"))))
  (let ((email (cadr (debian-changelog-last-maintainer))))
    (browse-url (concat "http://qa.debian.org/developer.php?login=" email))
    (message "Looking up developer summary page for %s via browse-url" email)))

;; co-maintenance as per bug #352957 by Luca Capello 2006
(defun debian-changelog-comaintainer-insert (name separator)
  "In the line before SEPARATOR, insert the co-maintainer name as for
the form [ NAME ]."
  (goto-char (point-min))
  (re-search-forward (concat "\n " separator))
  (previous-line 1)
  (insert "\n  [ " name " ]")
  (when (string= "--" separator)
    (insert "\n")))

(defun debian-changelog-comaintainer ()
  "If the last maintainer is different from the current one, create a
co-maintained changelog entry."
  (let ((name (car (debian-changelog-last-maintainer))))
    (unless (string= name debian-changelog-full-name)
      (let ((maintainers-found)
            (debian-changelog-last-entry-end
             (progn (goto-char (point-min))
                    (re-search-forward "\n --"))))
        (mapc (lambda (x)
                (goto-char (point-min))
                (when (search-forward x debian-changelog-last-entry-end t)
                  (add-to-list 'maintainers-found x)))
              (list name debian-changelog-full-name))
        ;; set the co-maintenance if any
        (if maintainers-found
            ;; co-maintenance, debian-changelog-full-name is not present
            (if (and (member name maintainers-found)
                     (not (member debian-changelog-full-name
                                  maintainers-found)))
                (debian-changelog-comaintainer-insert
                 debian-changelog-full-name "--"))
          ;; no co-maintenance
          (mapc (lambda (x)
                  (debian-changelog-comaintainer-insert (car x) (cadr x)))
                `((,name " *") (,debian-changelog-full-name "--"))))))))

;;
;; interactive function to unfinalise changelog (so modifications can be made)
;;

(defun debian-changelog-unfinalise-last-version ()
  "Remove the `finalisation' information.
Removes maintainer's name, email address and release date so that new entries
can be made."
  (interactive)
  (if (debian-changelog-finalised-p) nil
    (error "Most recent version is not finalised"))
  (save-excursion
    (debian-changelog-comaintainer)
    (goto-char (point-min))
    (re-search-forward "\n --")
    (let ((dels (point)))
      (end-of-line)
      (delete-region dels (point)))))

;;
;; top level interactive function to activate mode
;;

(defvar imenu-create-index-function)
;;;###autoload
(defun debian-changelog-mode ()
  "Major mode for editing Debian-style change logs.
Runs `debian-changelog-mode-hook' if it exists.

Key bindings:

\\{debian-changelog-mode-map}

If you want to use your debian.org email address for debian/changelog
entries without using it for the rest of your email, use the `customize`
interface to set it, or simply set the variable
`debian-changelog-mailing-address' in your ~/.emacs file, e.g.

 (setq debian-changelog-mailing-address \"myname@debian.org\"))"

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'debian-changelog-mode
        mode-name "Debian changelog"
        left-margin 2
        fill-prefix "  "
        fill-column 74)
  ;;(hack-local-variables)
  ;;  Can't hack-local-varibles because a "mode: " creates an infinite loop.
  ;;  It doesn't matter anyway.  The Local Variable block is parsed after
  ;;  the mode is run when visited by find-file.  That's the only time it's
  ;;  done.
  (use-local-map debian-changelog-mode-map)
  ;; Let each entry behave as one paragraph:
                                        ; (set (make-local-variable 'paragraph-start) "\\*")
                                        ; (set (make-local-variable 'paragraph-separate) "\\*\\|\\s-*$|\\S-")
  ;; PSG: The following appears to get fill-paragraph to finally work!
  (set (make-local-variable 'paragraph-start) "\\*\\|\\s *$\\|\f\\|^\\<")
  (set (make-local-variable 'paragraph-separate) "\\s *$\\|\f\\|^\\<")
  ;; Let each version behave as one page.
  ;; Match null string on the heading line so that the heading line
  ;; is grouped with what follows.
  (set (make-local-variable 'page-delimiter) "^\\<")
  (set (make-local-variable 'version-control) 'never)
  (set (make-local-variable 'adaptive-fill-regexp) "\\s *")
  (set (make-local-variable 'font-lock-defaults)
       '((debian-changelog-font-lock-keywords
          debian-changelog-font-lock-keywords-1
          debian-changelog-font-lock-keywords-2) t t))
  (set (make-local-variable
        'debian-changelog-local-variables-maybe-remove-done) nil)
  (set (make-local-variable 'indent-line-function) 'indent-relative-maybe)
  (set (make-local-variable 'outline-regexp) "^[a-z]")
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (debian-bug-bug-menu-init debian-changelog-mode-map)
  (easy-menu-add debian-changelog-menu)
  (cond
   (debian-changelog-use-imenu
    (require 'imenu)
    (setq imenu-create-index-function 'imenu--create-debian-changelog-index)
    (if (or window-system
            (fboundp 'tmm-menubar))
        (progn
          (imenu-add-to-menubar "History")
                                        ;(imenu-update-menubar)
          ))))
  (cond
   (debian-changelog-highlight-mouse-t
    (debian-changelog-setup-highlight-mouse-keymap)
    (debian-changelog-highlight-mouse)))
  (run-hooks 'debian-changelog-mode-hook))
;;(easy-menu-add debian-changelog-menu))

;;
;; font-lock face defs by Peter Galbraith

(defvar debian-changelog-warning-face 'debian-changelog-warning-face
  "Face to use for important keywords.")

(cond
 ((and (fboundp 'facep)
       (facep 'font-lock-warning-face))
  (copy-face 'font-lock-warning-face 'debian-changelog-warning-face))
 ((fboundp 'defface)
  (defface debian-changelog-warning-face
    '((((class grayscale)(background light))(:foreground "DimGray" :bold t))
      (((class grayscale)(background dark))(:foreground "LightGray" :bold t))
      (((class color)(background light))(:foreground "red" :bold t ))
      (((class color)(background dark))(:foreground "red" :bold t ))
      (t (:bold t)))
    "Face for debian-changelog important strings."
    :group 'debian-changelog-faces))
 (t
  ;;; XEmacs19:
  (make-face 'debian-changelog-warning-face
             "Face to use for important keywords.in debian-changelog-mode")
  (make-face-bold 'debian-changelog-warning-face)
  ;; XEmacs uses a tag-list thingy to determine if we are using color
  ;;  or mono (and I assume a dark background).
  (set-face-foreground 'debian-changelog-warning-face
                       "red" 'global nil 'append)))

;;
;; font-lock definition by Chris Waters,
;;           revisited by Peter Galbraith (Apr 2001)

;; Available faces:
;; keyword-face, type-face, string-face, comment-face,
;; variable-name-face, function-name-face
;; in emacs only:  builtin-face, constant-face, warning-face
;; in xemacs only: reference-face, doc-string-face, preprocessor-face

;; the mappings I've done below only use faces available in both emacsen.
;; this is somewhat limiting; I may consider adding my own faces later.

(defvar debian-changelog-font-lock-keywords-1
  (list
   ;; package name line: pkg (1.0-1) unstable; urgency=low
   '(debian-changelog-fontify-version
     (1 font-lock-function-name-face)
     (2 font-lock-type-face nil t)
     (3 font-lock-string-face nil t)
     (4 debian-changelog-warning-face nil t))
   '(debian-changelog-fontify-stable . debian-changelog-warning-face)
   '(debian-changelog-fontify-frozen . font-lock-type-face)
   '(debian-changelog-fontify-unstable . font-lock-string-face)
   '(debian-changelog-fontify-experimental . debian-changelog-warning-face)
   '(debian-changelog-fontify-unreleased . debian-changelog-warning-face)
   '(debian-changelog-fontify-urgency-crit . debian-changelog-warning-face)
   '(debian-changelog-fontify-urgency-high . debian-changelog-warning-face)
   '(debian-changelog-fontify-urgency-med . font-lock-type-face)
   '(debian-changelog-fontify-urgency-low . font-lock-string-face)
   ;; bug closers
   '(;"\\(closes:\\) *\\(\\(bug\\)?#? *[0-9]+\\(, *\\(bug\\)?#? *[0-9]+\\)*\\)"
     ;; Process lines that continue on multiple lines - Fred Bothamy
     "\\(closes:\\)[ \t\n]*\\(\\(bug\\)?#? *[0-9]+\\(,[ \t\n]*\\(bug\\)?#? *[0-9]+\\)*\\)"
     (1 font-lock-keyword-face)
     (2 debian-changelog-warning-face))
   '("^\t.*$" . debian-changelog-warning-face)
   ;; maintainer line (enforce 2 space exactly between email and date)
   '("^ -- \\(.+\\) <\\(.+@.+\\)>  \\([^ ].+\\)$"
     (1 font-lock-variable-name-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-string-face)))
  "First level highlighting for `debian-changelog-mode'.")

(defvar debian-changelog-font-lock-keywords-2
  (append
   debian-changelog-font-lock-keywords-1
   ;; bullet lines
   '(("^  +\\(\\*\\)" 1 font-lock-comment-face)))
  "High level highlighting for `debian-changelog-mode'.")

(defvar debian-changelog-font-lock-keywords
  debian-changelog-font-lock-keywords-1
  "Default expressions to highlight in `debian-changelog-mode'.")

;; Fontifier function by Peter Galbraith, Apr 24 2001

(defun debian-changelog-fontify-version (limit)
  "Return match for package name and version number up to LIMIT.
match 1 -> package name
      2 -> native vsn number
      3 -> non-native vsn number
      4 -> non-native NMU vsn number"
  (when (re-search-forward
;;; The following is not strictly correct.  The upstream version may actually
;;; contain a hyphen if a debian version number also exists, making two hyphens
;;; I'm assuming it begins with a digit, which is not enforced
         "^\\(\\S-+\\) (\\([0-9]:\\)?\\([0-9][0-9a-zA-Z.+:~]*\\)\\(-\\([0-9a-zA-Z.+~]+\\)\\)*)" nil t)
    ;;                                                                                           ^
    ;; Note the asterix above, allowing more than one hyphen in the version
    ;; number, but wrongly assuming that all of it is the Debian version
    ;; instead of only the bit past the last hyphen.  I might get NMUs wrongly
    ;; for version numbers with multiple hyphens.

    ;; match 1: package name
    ;; match 2: epoch, if it exists
    ;; match 3: upstream version number
    ;; match 4: debian version number exists if matched
    ;; match 5: debian version number
    (cond
     ((not (match-string 4))
      ;; No Debian version number -> Debian native package
      (store-match-data
       (list (match-beginning 1)(match-end 3)
             (match-beginning 1)(match-end 1)
             (match-beginning 3)(match-end 3)
             nil nil
             nil nil)))
     ((match-string 4)
      ;; Debian version number -> Let's see if NMU...
      (let* ((deb-vsn (match-string 5))
             (is-NMU (save-match-data (string-match "\\." deb-vsn))))
        (cond
         (is-NMU
          (store-match-data
           (list (match-beginning 1)(match-end 5)
                 (match-beginning 1)(match-end 1)
                 nil nil
                 nil nil
                 (match-beginning 3)(match-end 5))))
         (t
          (store-match-data
           (list (match-beginning 1)(match-end 5)
                 (match-beginning 1)(match-end 1)
                 nil nil
                 (match-beginning 3)(match-end 5)
                 nil nil)))))))
    t))

(defun debian-changelog-fontify-urgency-crit (limit)
  (when (re-search-forward "^\\sw.* (.+).*; \\(urgency=critical\\)" limit t)
    (store-match-data
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-urgency-high (limit)
  (when (re-search-forward "^\\sw.* (.+).*; \\(urgency=high\\)" limit t)
    (store-match-data
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-urgency-med (limit)
  (when (re-search-forward "^\\sw.* (.+).*; \\(urgency=medium\\)" limit t)
    (store-match-data
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-urgency-low (limit)
  (when (re-search-forward "^\\sw.* (.+).*; \\(urgency=low\\)" limit t)
    (store-match-data
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-stable (limit)
  (when (re-search-forward "^\\sw.* (.+).* \\(\\(old\\)?stable\\(-security\\)?\\)" limit t)
    (store-match-data
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-frozen (limit)
  (when (re-search-forward "^\\sw.* (.+).* \\(testing\\(-security\\)?\\|frozen\\|woody-proposed-updates\\)" limit t)
    (store-match-data
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-unstable (limit)
  (when (re-search-forward "^\\sw.* (.+).* \\(unstable\\)" limit t)
    (store-match-data
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-experimental (limit)
  (when (re-search-forward "^\\sw.* (.+).* \\(experimental\\)" limit t)
    (store-match-data
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-unreleased (limit)
  (when (re-search-forward "^\\sw.* (.+).* \\(UNRELEASED\\)" limit t)
    (store-match-data
     (list (match-beginning 1)(match-end 1)))
    t))

;;
;; browse-url interfaces, by Peter Galbraith, Feb 23 2001
;;

(defvar debian-changelog-is-XEmacs
  (not (null (save-match-data (string-match "XEmacs\\|Lucid" emacs-version)))))

(defvar debian-changelog-mouse-keymap nil
  "Keymap for mouse commands.")

(defun debian-changelog-setup-highlight-mouse-keymap ()
  (setq debian-changelog-mouse-keymap
   ;;; First, copy the local keymap so we don't have `disappearing' menus
   ;;; when the mouse is moved over a bug number.

   ;;; FIXME: Check out (mouse-major-mode-menu) to see how it grabs the local
   ;;;        menus to display.
        (let ((m (copy-keymap (current-local-map))))
          ;;          (cond
          ;;           ((and debian-changelog-use-imenu
          ;;                 (or window-system (fboundp 'tmm-menubar)))
          ;;            (imenu-add-to-menubar "History")))
          (cond
           (debian-changelog-is-XEmacs
            (set-keymap-name m 'debian-changelog-mouse-keymap)
            (define-key m [button3]
              'debian-bug-web-this-bug-under-mouse))
           (t
            (define-key m [down-mouse-3]
              'debian-bug-web-this-bug-under-mouse)))
          m)))

(defvar debian-changelog-ext-list nil
  "XEmacs buffer-local list of debian-changelog-cite extents.")
(make-variable-buffer-local 'debian-changelog-ext-list)
(put 'debian-changelog-ext-list 'permanent-local t)

(defun debian-changelog-highlight-mouse ()
  "Make that nice green highlight when the mouse is over a bug number.
Also set keymap."
  (interactive)
  (save-excursion
    (let ((s)(e)(extent)(local-extent-list debian-changelog-ext-list)
          (inhibit-read-only t)
          (modified (buffer-modified-p))) ;put-text-property changing this?
      ;; Remove the mouse face properties first.
      (setq debian-changelog-ext-list nil)              ;Reconstructed below...
      (if (string-match "XEmacs\\|Lucid" emacs-version)
          (while local-extent-list
            (setq extent (car local-extent-list))
            (if (or (extent-detached-p extent)
                    (and (<= (point-min)(extent-start-position extent))
                         (>= (point-max)(extent-end-position extent))))
                (delete-extent extent)
              (setq debian-changelog-ext-list
                    (cons extent debian-changelog-ext-list)))
            (setq local-extent-list (cdr local-extent-list)))
        ;; Remove properties for regular emacs
        ;; FIXME This detroys all mouse-faces and local-maps!
        (let ((before-change-functions) (after-change-functions))
          (remove-text-properties (point-min) (point-max)
                                  '(mouse-face t local-map t))))
      (goto-char (point-min))
      ;; FIXME: Ideally, I want to hightlight _only_ the digit parts
      ;; (skipping the coma, and the word "bug".
      (while
          (re-search-forward
;;;        "\\(closes:\\) *\\(\\(bug\\)?#? *[0-9]+\\(, *\\(bug\\)?#? *[0-9]+\\)*\\)"
           ;; Same deal as for font-lock - patch from Fred Bothamy.
           "\\(closes:\\)[ \t\n]*\\(\\(bug\\)?#? *[0-9]+\\(,[ \t\n]*\\(bug\\)?#? *[0-9]+\\)*\\)"
           nil t)
        (setq s (match-beginning 2))
        (setq e (match-end 2))
        (cond
         ((string-match "XEmacs\\|Lucid" emacs-version)
          (setq extent (make-extent s e))
          (setq debian-changelog-ext-list
                (cons extent debian-changelog-ext-list))
          (set-extent-property extent 'highlight t)
          (set-extent-property extent 'start-open t)
                                        ;         (set-extent-property extent 'balloon-help 'debian-changelog-label-help)
                                        ;         (set-extent-property extent 'help-echo 'debian-changelog-label-help-echo)
          (set-extent-property extent 'keymap debian-changelog-mouse-keymap))
         (t
          (let ((before-change-functions) (after-change-functions))
            (put-text-property s e 'local-map
                               debian-changelog-mouse-keymap)
            (put-text-property s e 'mouse-face 'highlight)))))
      (set-buffer-modified-p modified))))

;;;-------------
;;; imenu stuff - Peter Galbraith, May 2001

(eval-when-compile
  (require 'cl)
  (if (fboundp 'imenu)                    ;Make sure auto-load is loaded
      (require 'imenu)))

(defvar debian-changelog-imenu-doing-closebug nil
  "Internal flag set when imenu is processing many bug closings.")
(make-variable-buffer-local 'debian-changelog-imenu-doing-closebug)

(defun debian-changelog-imenu-prev-index-position-function ()
  (cond
   (debian-changelog-imenu-doing-closebug
    (if (not (posix-search-backward
              "\\(closes:\\)\\|[^0-9]\\([0-9]+\\)" nil t))
        nil                             ; No match
      ;; match 1 -> "closes:"
      ;; match 2 -> a bug number
      (cond
       ((match-string 1)
        (setq debian-changelog-imenu-doing-closebug nil)
        (debian-changelog-imenu-prev-index-position-function))
       (t
        ;; Return the bug number match
        t))))
   (t
    (if (not (re-search-backward
              "\\(closes: *\\(bug\\)?#? *[0-9]+\\)\\|\\(^\\sw.* (\\(.+\\))\\)"
              nil t))
        nil                             ; No match
      ;; match 1 -> "closes:"
      ;; match 4 -> a version number
      (cond
       ((match-string 1)
        (setq debian-changelog-imenu-doing-closebug t)
        (forward-char -1)
        (re-search-forward
         "\\(closes:\\) *\\(\\(bug\\)?#? *[0-9]+\\(, *\\(bug\\)?#? *[0-9]+\\)*\\)"
         nil t)
        (forward-char 1)
        (debian-changelog-imenu-prev-index-position-function))
       (t
        ;; Return the version number match
        t))))))

(defvar debian-changelog-imenu-counter nil
  "Debian-changelog-mode internal variable for imenu support.")

(defun imenu--create-debian-changelog-index ()
  (save-match-data
    (save-excursion
      (let ((index-alist '())
            (index-bug-alist '())
            (index-bugsorted-alist '())
            (prev-pos 0)
            (imenu-scanning-message "Scanning changelog for History (%3d%%)")
            )
        (setq debian-changelog-imenu-counter -99)
        (goto-char (point-max))
        (imenu-progress-message prev-pos 0 t)
;;;          (message "Scanning changelog history...")
        (setq debian-changelog-imenu-doing-closebug nil)
        (while (debian-changelog-imenu-prev-index-position-function)
          (imenu-progress-message prev-pos nil t)
          (let ((marker (make-marker)))
            (set-marker marker (point))
            (cond
             ((match-beginning 2)     ;bug number
              (push (cons (match-string-no-properties 2) marker)
                    index-bug-alist))
             ((match-beginning 4)     ;version number
              (push (cons (match-string-no-properties 4) marker)
                    index-alist)))))
        (imenu-progress-message prev-pos 100 t)
;;;       (message "Scanning changelog history... done.")
        (cond
         (index-bug-alist
          (push (cons "Closed Bugs (chrono)"
                      index-bug-alist)
                index-alist)
          (setq index-bugsorted-alist (copy-alist index-bug-alist))
          (push (cons "Closed Bugs (sorted)"
                      (sort index-bugsorted-alist
                            'debian-changelog-imenu-sort))
                index-alist)))
        index-alist))))

(defun debian-changelog-imenu-sort (el1 el2)
  "Predicate to compare labels in lists."
  (string< (car el2) (car el1) ))

;;; end of imenu stuff
;;;-------------

;;; Setup auto-mode-alist
;; (in case /etc/emacs/site-start.d/50dpkg-dev.el not used)
;;
;; Crib note: no need for "NEWS.Debian.gz" or "changelog.Debian.gz" entries
;; since jka-compr.el dispatches using the basename after uncompressing.

(add-to-list 'auto-mode-alist '("/debian/*NEWS" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("NEWS.Debian" . debian-changelog-mode))

;;(add-to-list 'auto-mode-alist '("/debian/changelog\\'" . debian-changelog-mode))
;;; Instead use this.  See http://bugs.debian.org/457047 by Trent W. Buck
;;; Valid package names spec is Debian Policy section 5.6.7
(add-to-list
 'auto-mode-alist
 '("/debian/\\([[:lower:][:digit:]][[:lower:][:digit:].+-]+\\.\\)?changelog\\'"
   . debian-changelog-mode))

(add-to-list 'auto-mode-alist '("changelog.Debian" . debian-changelog-mode))
;; For debchange
(add-to-list 'auto-mode-alist '("changelog.dch" . debian-changelog-mode))

;;;###autoload(add-to-list 'auto-mode-alist '("/debian/*NEWS" . debian-changelog-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("NEWS.Debian" . debian-changelog-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("/debian/\\([[:lower:][:digit:]][[:lower:][:digit:].+-]+\\.\\)?changelog\\'" . debian-changelog-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("changelog.Debian" . debian-changelog-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("changelog.dch" . debian-changelog-mode))

(provide 'debian-changelog-mode)

;;; debian-changelog-mode.el ends here
