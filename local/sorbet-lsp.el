;; -*- lexical-binding: t -*-
(defun sorbet-file-typed-p ()
  (and
   (save-excursion
     (goto-char (point-min))
     (re-search-forward "^#\\s *typed:\\s *\\(true\\|strong\\|strict\\)" nil t))
   (locate-dominating-file "." "extn.rb")))

(defun sorbet-lsp-ruby-maybe-start-hook ()
  (when (sorbet-file-typed-p)
    (lsp-sorbet-mode-enable)))
(add-hook 'ruby-mode-hook 'sorbet-lsp-ruby-maybe-start-hook)

(defun sorbet-lsp-before-open-hook ()
  (when (and lsp-enable-xref (lsp--capability "definitionProvider"))
    (setq-local xref-backend-functions (list #'lsp--xref-backend)))
  (lsp-client-register-uri-handler
   (lsp--workspace-client lsp--cur-workspace) "http" #'browse-url)
  (lsp-client-register-uri-handler
   (lsp--workspace-client lsp--cur-workspace) "https" #'browse-url))
(add-hook 'lsp-before-open-hook 'sorbet-lsp-before-open-hook)

(with-eval-after-load 'lsp-mode
  (lsp-define-stdio-client
   lsp-sorbet-mode
   "Ruby"
   (lsp-make-traverser "Gemfile")
   '("pay" "exec" "scripts/bin/typecheck" "--lsp" "--cache-dir=" "-v"))
  (setq lsp-ui-flycheck-live-reporting nil
        lsp-ui-sideline-enable nil
        lsp-highlight-symbol-at-point nil
        lsp-ui-doc-enable nil)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(require 'lsp-mode)

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

(defun sorbet-lsp-restart ()
  (interactive)
  (with-sc-2fa
   (lambda ()
     (let* ((payserver (expand-file-name "~/stripe/pay-server/"))
            (workspace (gethash payserver lsp--workspaces)))
       (when workspace
         (let ((lsp--cur-workspace workspace))
           (lsp--shutdown-cur-workspace))))
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (and
                (eq major-mode 'ruby-mode)
                (sorbet-file-typed-p))
           (lsp-mode -1)
           (when lsp--cur-workspace
             (condition-case nil
                 (lsp--shutdown-cur-workspace)
               (error (setq lsp--cur-workspace nil))))
           (lsp-sorbet-mode-enable)))))))

(provide 'sorbet-lsp)
