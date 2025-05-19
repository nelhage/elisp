;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "envrc" "20250401.1656"
  "Support for `direnv' that operates buffer-locally."
  '((emacs      "26.1")
    (inheritenv "0.1")
    (seq        "2.24"))
  :url "https://github.com/purcell/envrc"
  :commit "4ca2166ac72e756d314fc2348ce1c93d807c1a14"
  :revdesc "4ca2166ac72e"
  :keywords '("processes" "tools")
  :authors '(("Steve Purcell" . "steve@sanityinc.com"))
  :maintainers '(("Steve Purcell" . "steve@sanityinc.com")))
