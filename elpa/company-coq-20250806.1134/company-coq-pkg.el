;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "company-coq" "20250806.1134"
  "A collection of extensions for Proof General's Coq mode."
  '((cl-lib       "0.5")
    (dash         "2.12.1")
    (yasnippet    "0.11.0")
    (company      "0.8.12")
    (company-math "1.1"))
  :url "https://github.com/cpitclaudel/company-coq"
  :commit "78ed04ce39e925232a556d2077718cc7b215469c"
  :revdesc "78ed04ce39e9"
  :keywords '("convenience" "languages")
  :authors '(("Clément Pit-Claudel" . "clement.pitclaudel@live.com"))
  :maintainers '(("Clément Pit-Claudel" . "clement.pitclaudel@live.com")))
