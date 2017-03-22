;;; gnuplot-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "gnuplot-mode" "gnuplot-mode.el" (22608 43655
;;;;;;  0 0))
;;; Generated autoloads from gnuplot-mode.el

(autoload 'gnuplot-mode "gnuplot-mode" "\
Major mode for editing gnuplot files

\(fn)" t nil)

(dolist (pattern '("\\.gnuplot\\'" "\\.gp\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gnuplot-mode)))

(autoload 'gnuplot-compile "gnuplot-mode" "\
Runs gnuplot -persist as a synchronous process and passes the
current buffer to it.  Buffer must be visiting a file for it to
work.

\(fn)" t nil)

(autoload 'gnuplot-run-region "gnuplot-mode" "\
Send region to gnuplot, ensuring a final newline.  Doesn't
require buffer to be visiting a file.

\(fn START END)" t nil)

(autoload 'gnuplot-run-buffer "gnuplot-mode" "\
Send buffer to gnuplot, ensuring a final newline.  Doesn't
require buffer to be visiting a file.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gnuplot-mode-autoloads.el ends here
