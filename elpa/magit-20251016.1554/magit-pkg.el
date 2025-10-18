;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20251016.1554"
  "A Git porcelain inside Emacs."
  '((emacs         "28.1")
    (compat        "30.1")
    (cond-let      "0.1")
    (llama         "1.0")
    (magit-section "4.4")
    (seq           "2.24")
    (transient     "0.10")
    (with-editor   "3.4"))
  :url "https://github.com/magit/magit"
  :commit "d662ec107e74a05a0348ca75c89ebd127d658656"
  :revdesc "d662ec107e74"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
