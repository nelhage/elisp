;; -*- lexical-binding: t -*-
(require 'cl-seq)

(defun my-make-diagnostics-filter (remove-if)
  (lambda (param _workspace)
    (let* ((diagnostics (gethash "diagnostics" param))
           (filtered (cl-remove-if remove-if diagnostics)))
      (puthash "diagnostics" filtered param))
    param))

(defvar
  my-lsp-filter-hints
  (my-make-diagnostics-filter
   (lambda (diag)
     (let ((severity (gethash "severity" diag 0)))
       (>= severity 4))))
  "Function to filter LSP hint diagnostics")
