;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "flycheck-rust" "20251231.1617"
  "Flycheck: Rust additions and Cargo support."
  '((emacs     "27.1")
    (flycheck  "28")
    (dash      "2.13.0")
    (seq       "2.3")
    (let-alist "1.0.4"))
  :url "https://github.com/flycheck/flycheck-rust"
  :commit "b9db73a7a5980ca884d5dd0cbe79b3291a185972"
  :revdesc "b9db73a7a598"
  :keywords '("tools" "convenience")
  :authors '(("Sebastian Wiesner" . "swiesner@lunaryorn.com"))
  :maintainers '(("Sebastian Wiesner" . "swiesner@lunaryorn.com")))
