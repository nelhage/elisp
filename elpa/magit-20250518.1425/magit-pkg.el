;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250518.1425"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.1.0.0")
    (llama         "0.6.2")
    (magit-section "4.3.5")
    (seq           "2.24")
    (transient     "0.8.8")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "75c61919994a23494152f726102898456a6055b7"
  :revdesc "75c61919994a"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
