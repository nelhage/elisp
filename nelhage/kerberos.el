;; -*- mode: emacs-lisp; indent-tabs-mode: nil -*-

;; (require 'shell-wrap)

(defun kinit (&optional principle)
  (interactive)
  (if (not principle)
      (setq principle (user-login-name)))
  (let* ((passwd (read-passwd "Kerberos Password: "))
         (kinit (start-process "kinit" nil "kinit" "-45" principle)))
    (process-send-string kinit passwd)
    (process-send-string kinit "\n")))

;; (make-shell-wrapper
;; 'kinit "kinit"
;; ((principle . (user-login-name)))
;; ((password . 1)))


(provide 'kerberos)
