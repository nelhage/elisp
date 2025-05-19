;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "flycheck-rust" "20250226.2240"
  "Flycheck: Rust additions and Cargo support."
  '((emacs     "27.1")
    (flycheck  "28")
    (dash      "2.13.0")
    (seq       "2.3")
    (let-alist "1.0.4"))
  :url "https://github.com/flycheck/flycheck-rust"
  :commit "2b544bab19b987bfb41d5d88801b89e29bdf69c7"
  :revdesc "2b544bab19b9"
  :keywords '("tools" "convenience")
  :authors '(("Sebastian Wiesner" . "swiesner@lunaryorn.com"))
  :maintainers '(("Sebastian Wiesner" . "swiesner@lunaryorn.com")))
