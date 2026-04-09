;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "envrc" "20260325.1609"
  "Support for `direnv' that operates buffer-locally."
  '((emacs      "28.1")
    (inheritenv "0.1")
    (seq        "2.24"))
  :url "https://github.com/purcell/envrc"
  :commit "60820285aeed4f87969d663a1c14a905c7bb763a"
  :revdesc "60820285aeed"
  :keywords '("processes" "tools")
  :authors '(("Steve Purcell" . "steve@sanityinc.com"))
  :maintainers '(("Steve Purcell" . "steve@sanityinc.com")))
