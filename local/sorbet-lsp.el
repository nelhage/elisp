;; -*- lexical-binding: t -*-
(defun sorbet-file-typed-p ()
  (and
   (save-excursion
     (goto-char (point-min))
     (re-search-forward "^#\\s *typed:\\s *\\(true\\|strong\\|strict\\)" nil t))
   (locate-dominating-file "." "extn.rb")))

(defun sorbet-lsp-ruby-maybe-start-hook ()
  (when (sorbet-file-typed-p)
    (lsp)))
(add-hook 'ruby-mode-hook 'sorbet-lsp-ruby-maybe-start-hook)

(defvar lsp--cur-workspace)
(defvar lsp-ui-doc-position)

(defun sorbet-lsp-before-open-hook ()
  )
(add-hook 'lsp-before-open-hook 'sorbet-lsp-before-open-hook)

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection '("pay" "exec" "scripts/bin/typecheck" "--lsp"   "-v"
                            "--enable-experimental-lsp-go-to-definition"
                            "--enable-experimental-lsp-find-references"))
    :major-modes '(ruby-mode)
    :server-id 'rbls))
  (setq lsp-ui-doc-position 'at-point)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'lsp-mode-hook 'lsp-ui-doc-mode))

(require 'lsp-mode)
(require 'lsp-ui)

(defun sc-2fa-process-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (let ((start (point)))
        (insert string)
        (ansi-color-apply-on-region start (point))))))

(defun sc-2fa-make-process-filter (cb buffer)
  (lambda (proc exit-str)
    (let ((status (process-status proc)))
      (when (memq status '(exit signal))
        (kill-buffer buffer)
        (if (zerop (process-exit-status proc))
            (funcall cb)
          (message "sc-2fa: 2fa failed."))))))

(defun with-sc-2fa (cb)
  (if (zerop (call-process "sc-2fa" nil nil nil "test"))
      (funcall cb)
    (let* ((buffer (generate-new-buffer "*sc-2fa*"))
           (proc (start-process "sc-2fa" buffer "sc-2fa")))
      (display-buffer buffer)
      (set-process-filter proc #'sc-2fa-process-filter)
      (set-process-sentinel
       proc (sc-2fa-make-process-filter cb buffer))))
  nil)

; (with-sc-2fa #'(lambda () (message "2fa ok")))

(provide 'sorbet-lsp)
