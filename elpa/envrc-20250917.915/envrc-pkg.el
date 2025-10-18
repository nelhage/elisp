;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "envrc" "20250917.915"
  "Support for `direnv' that operates buffer-locally."
  '((emacs      "27.1")
    (inheritenv "0.1")
    (seq        "2.24"))
  :url "https://github.com/purcell/envrc"
  :commit "de1ae6e538764f74659f358b04af0d84fa0fef42"
  :revdesc "de1ae6e53876"
  :keywords '("processes" "tools")
  :authors '(("Steve Purcell" . "steve@sanityinc.com"))
  :maintainers '(("Steve Purcell" . "steve@sanityinc.com")))
