(defvar *javadoc-known-classes* (make-hash-table :test 'equal))
(defvar *javadoc-base-url* "http://java.sun.com/j2se/1.5.0/docs/api/")
(defvar *javadoc-class-history* nil)
(defvar *javadoc-save-file* "~/.elisp/nelhage/java-classes")

(require 'utils)

(defun read-class ()
  (let ((completions nil)
          (default (thing-at-point 'word)))
      (maphash (lambda (k v)
                 (setq completions
                       (cons
                        (cons k t)
                        (cons
                         (cons v t)
                         completions))))
               *javadoc-known-classes*)
      (completing-read
                 (if default
                     (concat "Class (default " default "): ")
                   "Class: ")
                 completions
                 nil nil nil
                 *javadoc-class-history*
                 default)))

(defun javadoc-lookup-class (&optional class)
  (interactive)
  (when (null class)
    (setq class (read-class)))
  (if (string-match "\\." class)
      (progn 
        (browse-url (concat *javadoc-base-url*
                          (replace-all "\\." "/" class)
                          ".html")))
    (let ((qualified-class (gethash class *javadoc-known-classes*)))
      (if qualified-class
          (javadoc-lookup-class qualified-class)
        (error "Unknown class -- %s" class)))))

(defun javadoc-remove-package (class)
  (string-match ".*\\.\\([^.]*\\)" class)
  (match-string 1 class))

(defun javadoc-cache-class-package (class &optional nosave)
  (unless (equal (gethash (javadoc-remove-package class)
                          *javadoc-known-classes*) class)
    (puthash (javadoc-remove-package class) class
             *javadoc-known-classes*)
    (unless nosave (javadoc-save-classes))))

(defun javadoc-load-classes () 
  (with-temp-buffer
    (insert-file-contents *javadoc-save-file*)
    (condition-case nil
        (while (setq obj (read (current-buffer)))
          (javadoc-cache-class-package (symbol-name obj) t))
      (end-of-file nil))))

(provide 'javadoc)
