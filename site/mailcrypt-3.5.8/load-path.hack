;; Uncomment the following line and edit it appropriately if
;; your installation of VM, RMAIL, MH-E, or GNUS is not in
;; the default load-path.

;; (setq load-path (cons "/users/patl/elisp" load-path))

;; This insures that (require 'mailcrypt) will work correctly while
;; byte-compiling.

(setq load-path (cons nil load-path))

;; Make sure user is

(if (and
     (not (string-match "^19\\." emacs-version))
     (not (string-match "^2[0-9]\\." emacs-version)))
    (message
     (concat 
      "\nWARNING - Mailcrypt requires at least version 19 of GNU Emacs.\n"
      "Your version is:\n"
      (emacs-version)
      "\n")))
