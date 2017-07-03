;;; smartparens-markdown.el --- Additional configuration for Markdown based modes.

;; Copyright (C) 2017 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 11th May 2017
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of Smartparens.

;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some additional configuration for Markdown based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-markdown)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.
;;
;; If you have good ideas about what should be added please file an
;; issue on the github tracker.
;;
;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)
(require 'markdown-mode)


(defun sp-gfm-electric-backquote-p (_id action _context)
  "Do not insert ```...``` pair if that would be handled by `markdown-electric-backquote'."
  (and (eq action 'insert)
       markdown-gfm-use-electric-backquote
       (sp--looking-back-p "^```")))

(sp-with-modes 'markdown-mode
  (sp-local-pair "```" "```"))

(sp-with-modes 'gfm-mode
  (sp-local-pair "`" "`" :unless '(:add sp-gfm-electric-backquote-p))
  (sp-local-pair "```" "```" :unless '(:add sp-gfm-electric-backquote-p)))

(provide 'smartparens-markdown)
;;; smartparens-markdown.el ends here
