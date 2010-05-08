
(provide 'psgml-sysdep)

(require 'psgml)
(cond
 (sgml-running-lucid
  (require 'psgml-lucid))
 (t
  (require 'psgml-other)))
