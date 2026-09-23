((ac-rtags :source "elpaca-menu-lock-file" :recipe
           (:package "ac-rtags" :repo "Andersbakken/rtags" :fetcher
                     github :version-regexp "use-header-instead"
                     :files ("src/ac-rtags.el") :source "MELPA" :id
                     ac-rtags :type git :protocol https :inherit t
                     :depth treeless :ref
                     "4abf149e27645dd2bd0e4f7557e114fa86822055"))
 (adaptive-wrap :source "elpaca-menu-lock-file" :recipe
                (:package "adaptive-wrap" :repo
                          ("https://github.com/emacsmirror/gnu_elpa"
                           . "adaptive-wrap")
                          :tar "0.9" :host gnu :branch
                          "externals/adaptive-wrap" :files
                          ("*" (:exclude ".git")) :source "GNU ELPA"
                          :id adaptive-wrap :type git :protocol https
                          :inherit t :depth treeless :ref
                          "e929b38c12f17aa6f8d6270326301d61fbb09cab"))
 (anaphora :source "elpaca-menu-lock-file" :recipe
           (:package "anaphora" :repo "rolandwalker/anaphora" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id anaphora :type git :protocol
                     https :inherit t :depth treeless :ref
                     "d22ae8afd3b3bf6a383f6a6c27522893b57130b1"))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo
                     "radian-software/apheleia" :files
                     (:defaults ("scripts" "scripts/formatters"))
                     :source "MELPA" :id apheleia :type git :protocol
                     https :inherit t :depth treeless :ref
                     "047119b7c6f18ffdbf5f88116a427f7e38653cc6"))
 (async :source "elpaca-menu-lock-file" :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :id async :type git :protocol https
                  :inherit t :depth treeless :ref
                  "4fdcb061a166e0d6ccc27d3829a28e04415ae825"))
 (auto-complete :source "elpaca-menu-lock-file" :recipe
                (:package "auto-complete" :fetcher github :repo
                          "auto-complete/auto-complete" :files
                          ("*.el" "dict") :source "MELPA" :id
                          auto-complete :type git :protocol https
                          :inherit t :depth treeless :ref
                          "8419bec94f41ae78518d948aec0fc761534d7771"))
 (base16-theme :source "elpaca-menu-lock-file" :recipe
               (:package "base16-theme" :repo
                         "tinted-theming/base16-emacs" :fetcher github
                         :files (:defaults "build/*.el") :source
                         "MELPA" :id base16-theme :type git :protocol
                         https :inherit t :depth treeless :ref
                         "c66ddd7415bddfecfc4e1095b50cde5391270375"))
 (bison-mode :source "elpaca-menu-lock-file" :recipe
             (:package "bison-mode" :repo "Wilfred/bison-mode"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id bison-mode :type git
                       :protocol https :inherit t :depth treeless :ref
                       "4f2e20394a475931409618c1635e9c9f1cf07d9c"))
 (caddyfile-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "caddyfile-mode" :fetcher github :repo
                           "Schnouki/caddyfile-mode" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :id caddyfile-mode :type
                           git :protocol https :inherit t :depth
                           treeless :ref
                           "fc41148f5a7eb320f070666f046fb9d88cf17680"))
 (chatgpt-shell :source "elpaca-menu-lock-file" :recipe
                (:package "chatgpt-shell" :fetcher github :repo
                          "xenodium/chatgpt-shell" :files
                          ("*.el"
                           (:exclude "test_chatgpt-shell.el"
                                     "shell-maker.el"
                                     "ob-chatgpt-shell.el"
                                     "dall-e-shell.el"
                                     "ob-dall-e-shell.el"))
                          :source "MELPA" :id chatgpt-shell :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "ed3bd8104c0e476b6cdf5787ba72b5e5dd1d39b0"))
 (clang-format :source "elpaca-menu-lock-file" :recipe
               (:package "clang-format" :fetcher github :repo
                         "emacsmirror/clang-format" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id clang-format :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "a099177b5cd5060597d454e4c1ffdc96b92ba985"))
 (clipetty :source "elpaca-menu-lock-file" :recipe
           (:package "clipetty" :repo "spudlyo/clipetty" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id clipetty :type git :protocol
                     https :inherit t :depth treeless :ref
                     "01b39044b9b65fa4ea7d3166f8b1ffab6f740362"))
 (clojure-mode :source "elpaca-menu-lock-file" :recipe
               (:package "clojure-mode" :repo
                         "clojure-emacs/clojure-mode" :fetcher github
                         :files ("clojure-mode.el") :source "MELPA"
                         :id clojure-mode :type git :protocol https
                         :inherit t :depth treeless :ref
                         "3c68569738f04a22d52f1ca28f593c2ee733bf04"))
 (coffee-mode :source "elpaca-menu-lock-file" :recipe
              (:package "coffee-mode" :repo "defunkt/coffee-mode"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id coffee-mode :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "1c6adeae14f3795d3b1e44321189ed7c2c93c967"))
 (company :source "elpaca-menu-lock-file" :recipe
          (:package "company" :fetcher github :repo
                    "company-mode/company-mode" :files
                    (:defaults "icons"
                               ("images/small"
                                "doc/images/small/*.png"))
                    :source "MELPA" :id company :type git :protocol
                    https :inherit t :depth treeless :ref
                    "1cc907ac9e46ae4209eb5a341131787e0c678406"))
 (company-coq :source "elpaca-menu-lock-file" :recipe
              (:package "company-coq" :repo "cpitclaudel/company-coq"
                        :fetcher github :files (:defaults "refman")
                        :source "MELPA" :id company-coq :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "1fc1d8f2d56e460b33c6d41a659488dce7b214f9"))
 (company-go :source "elpaca-menu-lock-file" :recipe
             (:package "company-go" :repo "emacsattic/company-go"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id company-go :type git
                       :protocol https :inherit t :depth treeless :ref
                       "31948b463f2fc18f8801e5a8fe511fef300eb3dd"))
 (company-math :source "elpaca-menu-lock-file" :recipe
               (:package "company-math" :fetcher github :repo
                         "vspinu/company-math" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id company-math :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "3eb006874e309ff4076d947fcbd61bb6806aa508"))
 (company-rtags :source "elpaca-menu-lock-file" :recipe
                (:package "company-rtags" :repo "Andersbakken/rtags"
                          :fetcher github :version-regexp
                          "use-header-instead" :files
                          ("src/company-rtags.el") :source "MELPA" :id
                          company-rtags :type git :protocol https
                          :inherit t :depth treeless :ref
                          "4abf149e27645dd2bd0e4f7557e114fa86822055"))
 (compat :source "elpaca-menu-lock-file" :recipe
         (:package "compat" :repo
                   ("https://github.com/emacs-compat/compat"
                    . "compat")
                   :tar "31.1.0.0" :host gnu :files
                   ("*" (:exclude ".git")) :source "GNU ELPA" :id
                   compat :type git :protocol https :inherit t :depth
                   treeless :ref
                   "90880f81419577e1d3f68424d2a3adf31e6d663e"))
 (cond-let :source "elpaca-menu-lock-file" :recipe
           (:package "cond-let" :fetcher github :repo
                     "tarsius/cond-let" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id cond-let :type git :protocol
                     https :inherit t :depth treeless :ref
                     "3b88187fe067d4ca3dec3ef8a329b0ce18bdb356"))
 (conda :source "elpaca-menu-lock-file" :recipe
        (:package "conda" :repo "necaris/conda.el" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :id conda :type git :protocol https
                  :inherit t :depth treeless :ref
                  "58b43d5020b8a7cc55065031bae9d0a5020528e0"))
 (counsel :source "elpaca-menu-lock-file" :recipe
          (:package "counsel" :repo "abo-abo/swiper" :fetcher github
                    :files ("counsel.el") :source "MELPA" :id counsel
                    :type git :protocol https :inherit t :depth
                    treeless :ref
                    "7b267e29cd47b2036eafbc212683a721bd2b7dd0"))
 (csv :source "elpaca-menu-lock-file" :recipe
      (:package "csv" :fetcher gitlab :repo "u11/csv.el" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id csv :type git :protocol https
                :inherit t :depth treeless :ref
                "8ed083c171a5e8caf11ebfbec67af3119ab1fd90"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
           (:package "csv-mode" :repo
                     ("https://github.com/emacsmirror/gnu_elpa"
                      . "csv-mode")
                     :tar "1.27" :host gnu :branch
                     "externals/csv-mode" :files
                     ("*" (:exclude ".git")) :source "GNU ELPA" :id
                     csv-mode :type git :protocol https :inherit t
                     :depth treeless :ref
                     "a693259ddef057eda82421b2a4410b3e35832e91"))
 (cuda-mode :source "elpaca-menu-lock-file" :recipe
            (:package "cuda-mode" :fetcher github :repo
                      "chachi/cuda-mode" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id cuda-mode :type git
                      :protocol https :inherit t :depth treeless :ref
                      "c3dae31b3d1abedf4d0b98840127e2cac73d6ad8"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "MELPA" :id dash
                 :type git :protocol https :inherit t :depth treeless
                 :ref "d746dd9edcb67a108818beb0cdc78dc1cb466832"))
 (dash-at-point :source "elpaca-menu-lock-file" :recipe
                (:package "dash-at-point" :fetcher github :repo
                          "stanaka/dash-at-point" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id dash-at-point :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "fba1a6f42ea51d05110e12c62bdced664059eb55"))
 (deferred :source "elpaca-menu-lock-file" :recipe
           (:package "deferred" :repo "kiwanami/emacs-deferred"
                     :fetcher github :files ("deferred.el") :source
                     "MELPA" :id deferred :type git :protocol https
                     :inherit t :depth treeless :ref
                     "2239671d94b38d92e9b28d4e12fd79814cfb9c16"))
 (dockerfile-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "dockerfile-mode" :fetcher github :repo
                            "spotify/dockerfile-mode" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :id dockerfile-mode :type
                            git :protocol https :inherit t :depth
                            treeless :ref
                            "97733ce074b1252c1270fd5e8a53d178b66668ed"))
 (ein :source "elpaca-menu-lock-file" :recipe
      (:package "ein" :repo "millejoh/emacs-ipython-notebook" :fetcher
                github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id ein :type git :protocol https
                :inherit t :depth treeless :ref
                "8fa836fcd1c22f45d36249b09590b32a890f2b9e"))
 (eldev :source "elpaca-menu-lock-file" :recipe
        (:package "eldev" :fetcher github :repo "emacs-eldev/eldev"
                  :files
                  (:defaults ("bin" "bin/*")
                             (:exclude "bin/*.in" "bin/*.part"))
                  :source "MELPA" :id eldev :type git :protocol https
                  :inherit t :depth treeless :ref
                  "129d26489b1749839b203ac40ae3cc609c737036"))
 (elgrep :source "elpaca-menu-lock-file" :recipe
         (:package "elgrep" :repo "TobiasZawada/elgrep" :fetcher
                   github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id elgrep :type git :protocol
                   https :inherit t :depth treeless :ref
                   "329eaf2e9e994e5535c7f7fe2685ec21d8323384"))
 (elixir-mode :source "elpaca-menu-lock-file" :recipe
              (:package "elixir-mode" :fetcher github :repo
                        "elixir-editors/emacs-elixir" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id elixir-mode :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "00d6580a040a750e019218f9392cf9a4c2dac23a"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source "Init file" :package "elpaca" :id elpaca :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "78b8e7cc98c198c8dbeb18140649e2d668126712" :depth 1
            :inherit ignore :files
            (:defaults "elpaca-test.el" (:exclude "extensions"))
            :build (:not elpaca-activate) :type git :protocol https))
 (envrc :source "elpaca-menu-lock-file" :recipe
        (:package "envrc" :fetcher github :repo "purcell/envrc" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :id envrc :type git :protocol https
                  :inherit t :depth treeless :ref
                  "1ecb82e01745d700578754eb35d6c1758290b869"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher
                                 github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info"
                                  "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi"
                                  "doc/*.texinfo" "lisp/*.el"
                                  "docs/dir" "docs/*.info"
                                  "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE"
                                            "README*" "*-pkg.el"))
                                 :source "MELPA" :id
                                 exec-path-from-shell :type git
                                 :protocol https :inherit t :depth
                                 treeless :ref
                                 "6146fdc16e9882df270be7e58ae8d628032d6bc4"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "MELPA" :id f :type git :protocol https :inherit
              t :depth treeless :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id flycheck :type git :protocol
                     https :inherit t :depth treeless :ref
                     "9076d01a8a685bb0f03302e7bf3feaf41e6c34dc"))
 (flycheck-aspell :source "elpaca-menu-lock-file" :recipe
                  (:package "flycheck-aspell" :fetcher github :repo
                            "leotaku/flycheck-aspell" :files
                            ("flycheck-aspell.el") :source "MELPA" :id
                            flycheck-aspell :type git :protocol https
                            :inherit t :depth treeless :ref
                            "abbac0f6ccd94224f19c70d8545fddfe9c27351f"))
 (flycheck-clangcheck :source "elpaca-menu-lock-file" :recipe
                      (:package "flycheck-clangcheck" :fetcher github
                                :repo "kumar8600/flycheck-clangcheck"
                                :files
                                ("*.el" "*.el.in" "dir" "*.info"
                                 "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 "docs/dir" "docs/*.info"
                                 "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el"
                                           "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "MELPA" :id
                                flycheck-clangcheck :type git
                                :protocol https :inherit t :depth
                                treeless :ref
                                "24a9424c484420073a24443a829fd5779752362b"))
 (flycheck-elixir :source "elpaca-menu-lock-file" :recipe
                  (:package "flycheck-elixir" :fetcher github :repo
                            "lbolla/emacs-flycheck-elixir" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :id flycheck-elixir :type
                            git :protocol https :inherit t :depth
                            treeless :ref
                            "b57a77a21d6cf9621b3387831cba34135c4fa35d"))
 (flycheck-julia :source "elpaca-menu-lock-file" :recipe
                 (:package "flycheck-julia" :repo
                           "gdkrmr/flycheck-julia" :fetcher github
                           :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :id flycheck-julia :type
                           git :protocol https :inherit t :depth
                           treeless :ref
                           "213b60a5a9a1cb7887260e1d159b5bb27167cbb6"))
 (flycheck-ocaml :source "elpaca-menu-lock-file" :recipe
                 (:package "flycheck-ocaml" :fetcher github :repo
                           "flycheck/flycheck-ocaml" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :id flycheck-ocaml :type
                           git :protocol https :inherit t :depth
                           treeless :ref
                           "e302792ba72ad57b262e1ab79799960d49a4278d"))
 (flycheck-rust :source "elpaca-menu-lock-file" :recipe
                (:package "flycheck-rust" :repo
                          "flycheck/flycheck-rust" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id flycheck-rust :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "42a63a9ec969d77f5c56e101833546e1afb0d884"))
 (fuzzy :source "elpaca-menu-lock-file" :recipe
        (:package "fuzzy" :fetcher github :repo
                  "auto-complete/fuzzy-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :id fuzzy :type git :protocol https
                  :inherit t :depth treeless :ref
                  "f92461a5eb55934a20149745fb0ccb1805222af8"))
 (gh :source "elpaca-menu-lock-file" :recipe
     (:package "gh" :repo "sigma/gh.el" :fetcher github :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "MELPA" :id gh :type git :protocol https
               :inherit t :depth treeless :ref
               "b1551245d3404eac6394abaebe1a9e0b2c504235"))
 (git-link :source "elpaca-menu-lock-file" :recipe
           (:package "git-link" :fetcher github :repo "sshaw/git-link"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id git-link :type git :protocol
                     https :inherit t :depth treeless :ref
                     "ca01d013bd575710e2cd47001ee1ef6ee41667cf"))
 (go-mode :source "elpaca-menu-lock-file" :recipe
          (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher
                    github :files ("go-mode.el") :source "MELPA" :id
                    go-mode :type git :protocol https :inherit t
                    :depth treeless :ref
                    "3a71d28ab47df685e54ca6046a7a3dd3e28b682c"))
 (gptel :source "elpaca-menu-lock-file" :recipe
        (:package "gptel" :repo "karthink/gptel" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :id gptel :type git :protocol https
                  :inherit t :depth treeless :ref
                  "44deae37928175764bbe8314dbbc79bdec28ab4c"))
 (graphviz-dot-mode :source "elpaca-menu-lock-file" :recipe
                    (:package "graphviz-dot-mode" :repo
                              "ppareit/graphviz-dot-mode" :fetcher
                              github :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :id graphviz-dot-mode
                              :type git :protocol https :inherit t
                              :depth treeless :ref
                              "0a4509e9f63c8eae8d050acc62eac642fd77ae59"))
 (haskell-mode :source "elpaca-menu-lock-file" :recipe
               (:package "haskell-mode" :repo "haskell/haskell-mode"
                         :fetcher github :files
                         (:defaults "NEWS" "logo.svg") :source "MELPA"
                         :id haskell-mode :type git :protocol https
                         :inherit t :depth treeless :ref
                         "4bdd38c22d8a54d3284b517e889863a1d3971998"))
 (hcl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "hcl-mode" :repo "hcl-emacs/hcl-mode" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id hcl-mode :type git :protocol
                     https :inherit t :depth treeless :ref
                     "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed"))
 (helm :source "elpaca-menu-lock-file" :recipe
       (:package "helm" :fetcher github :repo "emacs-helm/helm" :files
                 (:defaults "emacs-helm.sh"
                            (:exclude "helm-lib.el" "helm-source.el"
                                      "helm-multi-match.el"
                                      "helm-core.el"))
                 :source "MELPA" :id helm :type git :protocol https
                 :inherit t :depth treeless :ref
                 "228879977563003631edc67926e6a241f8523d73"))
 (helm-core :source "elpaca-menu-lock-file" :recipe
            (:package "helm-core" :repo "emacs-helm/helm" :fetcher
                      github :files
                      ("helm-core.el" "helm-lib.el" "helm-source.el"
                       "helm-multi-match.el")
                      :source "MELPA" :id helm-core :type git
                      :protocol https :inherit t :depth treeless :ref
                      "228879977563003631edc67926e6a241f8523d73"))
 (helm-ls-git :source "elpaca-menu-lock-file" :recipe
              (:package "helm-ls-git" :repo "emacs-helm/helm-ls-git"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id helm-ls-git :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "dd0ed5847d4bf1b27e767cf194475ada88ee8898"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "MELPA" :id ht :type git :protocol https
               :inherit t :depth treeless :ref
               "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (inf-ruby :source "elpaca-menu-lock-file" :recipe
           (:package "inf-ruby" :repo "nonsequitur/inf-ruby" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id inf-ruby :type git :protocol
                     https :inherit t :depth treeless :ref
                     "274398a24288a7db430a656b580ffbf889ca02aa"))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
             (:package "inheritenv" :fetcher github :repo
                       "purcell/inheritenv" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id inheritenv :type git
                       :protocol https :inherit t :depth treeless :ref
                       "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (ivy :source "elpaca-menu-lock-file" :recipe
      (:package "ivy" :repo "abo-abo/swiper" :fetcher github :files
                (:defaults "doc/ivy-help.org"
                           (:exclude "swiper.el" "counsel.el"
                                     "ivy-hydra.el" "ivy-avy.el"))
                :source "MELPA" :id ivy :type git :protocol https
                :inherit t :depth treeless :ref
                "7b267e29cd47b2036eafbc212683a721bd2b7dd0"))
 (javaimp :source "elpaca-menu-lock-file" :recipe
          (:package "javaimp" :repo
                    ("https://github.com/emacsmirror/gnu_elpa"
                     . "javaimp")
                    :tar "0.9.2" :host gnu :branch "externals/javaimp"
                    :files ("*" (:exclude ".git")) :source "GNU ELPA"
                    :id javaimp :type git :protocol https :inherit t
                    :depth treeless :ref
                    "b24e7d43f668cf731f57c3e52373180febabec45"))
 (jinja2-mode :source "elpaca-menu-lock-file" :recipe
              (:package "jinja2-mode" :fetcher github :repo
                        "paradoxxxzero/jinja2-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id jinja2-mode :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "03e5430a7efe1d163a16beaf3c82c5fd2c2caee1"))
 (js2-mode :source "elpaca-menu-lock-file" :recipe
           (:package "js2-mode" :repo "mooz/js2-mode" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id js2-mode :type git :protocol
                     https :inherit t :depth treeless :ref
                     "41d0e7f5ef51109c682016baa6fc6846e03e8517"))
 (jsonian :source "elpaca-menu-lock-file" :recipe
          (:package "jsonian" :fetcher github :repo "iwahbe/jsonian"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id jsonian :type git :protocol
                    https :inherit t :depth treeless :ref
                    "2709fb0140c92eb183c849fdc530fd59f4e4fd3d"))
 (julia-mode :source "elpaca-menu-lock-file" :recipe
             (:package "julia-mode" :repo
                       "JuliaEditorSupport/julia-emacs" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id julia-mode :type git
                       :protocol https :inherit t :depth treeless :ref
                       "1b5a4c2f5b7c3f842785985bf8778b8805cc6766"))
 (julia-repl :source "elpaca-menu-lock-file" :recipe
             (:package "julia-repl" :fetcher github :repo
                       "tpapp/julia-repl" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id julia-repl :type git
                       :protocol https :inherit t :depth treeless :ref
                       "7c818163388c732aeaee0b629b4559e5087c00ba"))
 (kkp :source "elpaca-menu-lock-file" :recipe
      (:package "kkp" :fetcher github :repo "benotn/kkp" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id kkp :type git :protocol https
                :inherit t :depth treeless :ref
                "82b7443e10a2ba287467b62e90b6adb6dd93dc99"))
 (kotlin-ts-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "kotlin-ts-mode" :fetcher gitlab :repo
                           "bricka/emacs-kotlin-ts-mode" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :id kotlin-ts-mode :type
                           git :protocol https :inherit t :depth
                           treeless :ref
                           "292c9a05ff2243c39a48201b277840b1d25e1ea0"))
 (lean4-mode :source "elpaca-menu-lock-file" :recipe
             (:source "Init file" :package "lean4-mode" :id lean4-mode
                      :type git :host github :repo
                      "leanprover-community/lean4-mode" :protocol
                      https :inherit t :depth treeless :ref
                      "d5ed4b1610de45d265fded03b9b1af904efd6c03"))
 (ledger-mode :source "elpaca-menu-lock-file" :recipe
              (:package "ledger-mode" :fetcher github :repo
                        "ledger/ledger-mode" :files
                        ("ledger-*.el" "doc/*.texi") :old-names
                        (ldg-mode) :source "MELPA" :id ledger-mode
                        :type git :protocol https :inherit t :depth
                        treeless :ref
                        "0dd5947e030d005efe1888331e84bd5a7e3e79b5"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source "MELPA" :id
                  llama :type git :protocol https :inherit t :depth
                  treeless :ref
                  "cfea618f14bc8317f8e4947fe10000b229b9a447"))
 (logito :source "elpaca-menu-lock-file" :recipe
         (:package "logito" :repo "sigma/logito" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id logito :type git :protocol
                   https :inherit t :depth treeless :ref
                   "d5934ce10ba3a70d3fcfb94d742ce3b9136ce124"))
 (loop :source "elpaca-menu-lock-file" :recipe
       (:package "loop" :repo "Wilfred/loop.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id loop :type git :protocol https
                 :inherit t :depth treeless :ref
                 "9db6372791bbd0cf3fa907ed0ae3e6b7bcf6cc57"))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher
                     github :files (:defaults "clients/*.*") :source
                     "MELPA" :id lsp-mode :type git :protocol https
                     :inherit t :depth treeless :ref
                     "d0835388732fcd39e951173ac1f6792cdf346706"))
 (lsp-pyright :source "elpaca-menu-lock-file" :recipe
              (:package "lsp-pyright" :repo "emacs-lsp/lsp-pyright"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id lsp-pyright :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "147d6d9c799f945ba4a2fb14824ddea9ade6de74"))
 (lsp-python-ms :source "elpaca-menu-lock-file" :recipe
                (:package "lsp-python-ms" :fetcher github :repo
                          "emacs-lsp/lsp-python-ms" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id lsp-python-ms :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "7bda327bec7b219d140c34dab4b1e1fbd41bc516"))
 (lsp-ui :source "elpaca-menu-lock-file" :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github
                   :files (:defaults "lsp-ui-doc.html" "resources")
                   :source "MELPA" :id lsp-ui :type git :protocol
                   https :inherit t :depth treeless :ref
                   "176eca71d1c5498ed6258b5b27d73293ff7cd7ed"))
 (lua-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher
                     github :files
                     (:defaults (:exclude "init-tryout.el")) :source
                     "MELPA" :id lua-mode :type git :protocol https
                     :inherit t :depth treeless :ref
                     "2f6b8d7a6317e42c953c5119b0119ddb337e0a5f"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el") :source "MELPA" :id lv :type git :protocol
               https :inherit t :depth treeless :ref
               "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   ("githooks" "githooks/*")
                   ("git-hooks" "git-hooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "MELPA" :id magit :type git :protocol https
                  :inherit t :depth treeless :ref
                  "cd9117e3a8eb9b50814474d5addfd2ca92af5605"))
 (magit-gerrit :source "elpaca-menu-lock-file" :recipe
               (:package "magit-gerrit" :repo
                         "emacsorphanage/magit-gerrit" :fetcher github
                         :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id magit-gerrit :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "d95d6d3febf7f9c04a4abefa3640610aae626683"))
 (magit-gh-pulls :source "elpaca-menu-lock-file" :recipe
                 (:package "magit-gh-pulls" :fetcher github :repo
                           "sigma/magit-gh-pulls" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :id magit-gh-pulls :type
                           git :protocol https :inherit t :depth
                           treeless :ref
                           "57f3a5158bbc7bfd169ee136fde351cce999e0ca"))
 (magit-popup :source "elpaca-menu-lock-file" :recipe
              (:package "magit-popup" :fetcher github :repo
                        "emacsattic/magit-popup" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id magit-popup :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "d8585fa39f88956963d877b921322530257ba9f5"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo
                          "magit/magit" :files
                          ("lisp/magit-section.el"
                           "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "MELPA" :id magit-section :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "cd9117e3a8eb9b50814474d5addfd2ca92af5605"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id markdown-mode :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "76cb4ffecfdf95ee769e5cb4608e04202c3c1521"))
 (marshal :source "elpaca-menu-lock-file" :recipe
          (:package "marshal" :fetcher github :repo "sigma/marshal.el"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id marshal :type git :protocol
                    https :inherit t :depth treeless :ref
                    "bc00044d9073482f589aad959e34d563598f682a"))
 (math-symbol-lists :source "elpaca-menu-lock-file" :recipe
                    (:package "math-symbol-lists" :fetcher github
                              :repo "vspinu/math-symbol-lists" :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :id math-symbol-lists
                              :type git :protocol https :inherit t
                              :depth treeless :ref
                              "ac3eb053d3b576fcdd192b0ac6ad5090ea3a7079"))
 (merlin :source "elpaca-menu-lock-file" :recipe
         (:package "merlin" :fetcher github :repo "ocaml/merlin"
                   :files
                   ("emacs/*.el"
                    (:exclude "emacs/merlin-ac.el"
                              "emacs/merlin-company.el"
                              "emacs/merlin-iedit.el"))
                   :source "MELPA" :id merlin :type git :protocol
                   https :inherit t :depth treeless :ref
                   "a93babce80d36da63455224efcf3c8ca68ab7916"))
 (mistty :source "elpaca-menu-lock-file" :recipe
         (:package "mistty" :fetcher github :repo "szermatt/mistty"
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id mistty :type git :protocol
                   https :inherit t :depth treeless :ref
                   "37fd8765e112f3125f2500a1e08328066dd56b19"))
 (mode-line-bell :source "elpaca-menu-lock-file" :recipe
                 (:package "mode-line-bell" :fetcher github :repo
                           "purcell/mode-line-bell" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :id mode-line-bell :type
                           git :protocol https :inherit t :depth
                           treeless :ref
                           "b888963bf390b6e1d132b72b8a14041bd7afae4b"))
 (ninja-mode :source "elpaca-menu-lock-file" :recipe
             (:package "ninja-mode" :fetcher github :repo
                       "ninja-build/ninja-emacs" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id ninja-mode :type git
                       :protocol https :inherit t :depth treeless :ref
                       "573c3aaedc6e90e9a8954bb70a24e079af7df390"))
 (nix-mode :source "elpaca-menu-lock-file" :recipe
           (:package "nix-mode" :fetcher github :repo "NixOS/nix-mode"
                     :files
                     (:defaults
                      (:exclude "nix-company.el" "nix-mode-mmm.el"))
                     :source "MELPA" :id nix-mode :type git :protocol
                     https :inherit t :depth treeless :ref
                     "2c77e7e0b7540efbb20ccaee3557ef90a5dc77f0"))
 (obsidian :source "elpaca-menu-lock-file" :recipe
           (:package "obsidian" :repo "licht1stein/obsidian.el"
                     :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id obsidian :type file :main
                     "/Users/nelhage/.emacs.d/home-manager/obsidian.el"
                     :protocol https :inherit t :depth treeless :ref
                     nil))
 (pcache :source "elpaca-menu-lock-file" :recipe
         (:package "pcache" :repo "sigma/pcache" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id pcache :type git :protocol
                   https :inherit t :depth treeless :ref
                   "17d785afa4532043afa8b2dc9ae3d9733528e758"))
 (polymode :source "elpaca-menu-lock-file" :recipe
           (:package "polymode" :fetcher github :repo
                     "polymode/polymode" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id polymode :type git :protocol
                     https :inherit t :depth treeless :ref
                     "8cb72fa5dcc0d98746c680043dc121edc7621e3a"))
 (popup :source "elpaca-menu-lock-file" :recipe
        (:package "popup" :fetcher github :repo
                  "auto-complete/popup-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :id popup :type git :protocol https
                  :inherit t :depth treeless :ref
                  "3afe431e9aa2e271aaf0412cbb50c733387e8ea4"))
 (popwin :source "elpaca-menu-lock-file" :recipe
         (:package "popwin" :fetcher github :repo
                   "emacsorphanage/popwin" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id popwin :type git :protocol
                   https :inherit t :depth treeless :ref
                   "b67254bef763ffa5ab781460bc47d6adf6f87127"))
 (posframe :source "elpaca-menu-lock-file" :recipe
           (:package "posframe" :fetcher github :repo
                     "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id posframe :type git :protocol
                     https :inherit t :depth treeless :ref
                     "bdabcec96f127b2daa2f8bf988a71ec146e301d5"))
 (prettier-js :source "elpaca-menu-lock-file" :recipe
              (:package "prettier-js" :repo "prettier/prettier-emacs"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id prettier-js :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "29ea00ae63d2b45b5ae86a46a190924f2d589f2c"))
 (protobuf-mode :source "elpaca-menu-lock-file" :recipe
                (:package "protobuf-mode" :fetcher github :repo
                          "protocolbuffers/protobuf" :files
                          ("editors/protobuf-mode.el") :source "MELPA"
                          :id protobuf-mode :type git :protocol https
                          :inherit t :depth treeless :ref
                          "57c7fbf6679cbde32c5c746649ee4a49fdb410d6"))
 (py-isort :source "elpaca-menu-lock-file" :recipe
           (:package "py-isort" :repo "paetzke/py-isort.el" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id py-isort :type git :protocol
                     https :inherit t :depth treeless :ref
                     "e67306f459c47c53a65604e4eea88a3914596560"))
 (pythonic :source "elpaca-menu-lock-file" :recipe
           (:package "pythonic" :fetcher github :repo
                     "pythonic-emacs/pythonic" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id pythonic :type git :protocol
                     https :inherit t :depth treeless :ref
                     "6036d5e3eeef534946de2adf0775caebd7cba45e"))
 (reformatter :source "elpaca-menu-lock-file" :recipe
              (:package "reformatter" :repo
                        "purcell/emacs-reformatter" :fetcher github
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id reformatter :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "2bd8818f3f2119a3876e574a437495214c87bc81"))
 (request :source "elpaca-menu-lock-file"
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github
             :files ("request.el") :source "MELPA" :id request :type
             git :protocol https :inherit t :depth treeless :ref
             "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6"))
 (rtags :source "elpaca-menu-lock-file" :recipe
        (:package "rtags" :repo "Andersbakken/rtags" :fetcher github
                  :version-regexp "use-header-instead" :files
                  ("src/rtags.el") :source "MELPA" :id rtags :type git
                  :protocol https :inherit t :depth treeless :ref
                  "4abf149e27645dd2bd0e4f7557e114fa86822055"))
 (ruby-electric :source "elpaca-menu-lock-file" :recipe
                (:package "ruby-electric" :fetcher github :repo
                          "ruby/elisp-ruby-electric" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id ruby-electric :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "c53376da891713e0c49f01aad2ff64d4fbb0b812"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "MELPA" :id s :type git :protocol https :inherit
              t :depth treeless :ref
              "d7c04b84d03481a1ed62ee13dbe595224ccbe57c"))
 (scala-mode :source "elpaca-menu-lock-file" :recipe
             (:package "scala-mode" :fetcher github :repo
                       "hvesalai/emacs-scala-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id scala-mode :type git
                       :protocol https :inherit t :depth treeless :ref
                       "50bcafa181baec7054e27f4bca55d5f9277c6350"))
 (seq :source "elpaca-menu-lock-file" :recipe
      (:package "seq" :repo
                ("https://github.com/emacsmirror/gnu_elpa" . "seq")
                :tar "2.24" :host gnu :branch "externals/seq" :files
                ("*" (:exclude ".git")) :source "GNU ELPA" :id seq
                :type git :protocol https :inherit t :depth treeless
                :ref "27a90793a13f149121180e864fa53d68b9eac0b3"))
 (shell-maker :source "elpaca-menu-lock-file" :recipe
              (:package "shell-maker" :fetcher github :repo
                        "xenodium/shell-maker" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id shell-maker :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "f448a74a8eded23aa42f8d60a41c5d8d3a183d07"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo
                    ("https://github.com/Malabarba/spinner.el"
                     . "spinner")
                    :tar "1.7.4" :host gnu :files
                    ("*" (:exclude ".git")) :source "GNU ELPA" :id
                    spinner :type git :protocol https :inherit t
                    :depth treeless :ref
                    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (svelte-mode :source "elpaca-menu-lock-file" :recipe
              (:package "svelte-mode" :fetcher github :repo
                        "leafOfTree/svelte-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id svelte-mode :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "ac8fba901dc790976f9893e338c8ad1241b897c6"))
 (swiper :source "elpaca-menu-lock-file" :recipe
         (:package "swiper" :repo "abo-abo/swiper" :fetcher github
                   :files ("swiper.el") :source "MELPA" :id swiper
                   :type git :protocol https :inherit t :depth
                   treeless :ref
                   "7b267e29cd47b2036eafbc212683a721bd2b7dd0"))
 (terraform-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "terraform-mode" :repo
                           "hcl-emacs/terraform-mode" :fetcher github
                           :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :id terraform-mode :type
                           git :protocol https :inherit t :depth
                           treeless :ref
                           "01635df3625c0cec2bb4613a6f920b8569d41009"))
 (typst-ts-mode :source "elpaca-menu-lock-file" :recipe
                (:package "typst-ts-mode" :repo
                          ("https://codeberg.org/meow_king/typst-ts-mode"
                           . "typst-ts-mode")
                          :tar "0.12.2" :host nongnu :files
                          ("*" (:exclude ".git")) :source
                          "NonGNU ELPA" :id typst-ts-mode :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "155bb36cff3afe701a0f6b57bd3fe5c9effaa314"))
 (verilog-mode :source "elpaca-menu-lock-file" :recipe
               (:package "verilog-mode" :repo
                         ("https://github.com/emacs-mirror/emacs"
                          . "verilog-mode")
                         :tar "2026.4.14.10117132" :host gnu :branch
                         "master" :files
                         ("lisp/progmodes/verilog-mode.el"
                          (:exclude ".git"))
                         :source "GNU ELPA" :id verilog-mode :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "891dad80905801515b7eb63f3ca0ab69be0e14cf"))
 (visual-fill-column :source "elpaca-menu-lock-file" :recipe
                     (:package "visual-fill-column" :fetcher codeberg
                               :repo "joostkremers/visual-fill-column"
                               :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :id visual-fill-column
                               :type git :protocol https :inherit t
                               :depth treeless :ref
                               "e1be9a1545157d24454d950c0ac79553c540edb7"))
 (vterm :source "elpaca-menu-lock-file" :recipe
        (:package "vterm" :fetcher github :repo
                  "akermu/emacs-libvterm" :files
                  ("CMakeLists.txt" "elisp.c" "elisp.h"
                   "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el"
                   "vterm-module.c" "vterm-module.h")
                  :source "MELPA" :id vterm :type git :protocol https
                  :inherit t :depth treeless :ref
                  "6d715a93fa0e5182bc137d4db09f376e06938aa5"))
 (web-mode :source "elpaca-menu-lock-file" :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id web-mode :type git :protocol
                     https :inherit t :depth treeless :ref
                     "ce24723eb900c455b488d224910519bd36af580a"))
 (websocket :source "elpaca-menu-lock-file" :recipe
            (:package "websocket" :repo "ahyatt/emacs-websocket"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id websocket :type git
                      :protocol https :inherit t :depth treeless :ref
                      "2195e1247ecb04c30321702aa5f5618a51c329c5"))
 (wfnames :source "elpaca-menu-lock-file" :recipe
          (:package "wfnames" :fetcher github :repo
                    "thierryvolpiatto/wfnames" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id wfnames :type git :protocol
                    https :inherit t :depth treeless :ref
                    "d8839fa42a24f7c781cd2d8c3f40eda31faa19be"))
 (window-number :source "elpaca-menu-lock-file" :recipe
                (:package "window-number" :repo
                          "nikolas/window-number" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id window-number :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "d41722de646ffeb3f70d26e4a86a5a1ba5c6be87"))
 (with-editor :source "elpaca-menu-lock-file" :recipe
              (:package "with-editor" :fetcher github :repo
                        "magit/with-editor" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id with-editor :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "3195a545b6c9bec7f3fbb68eaba14a172e0ea3ef"))
 (yaml :source "elpaca-menu-lock-file" :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id yaml :type git :protocol https
                 :inherit t :depth treeless :ref
                 "5546f36bde24a9a8c1934e0f6ce205cd41d72537"))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id yaml-mode :type git
                      :protocol https :inherit t :depth treeless :ref
                      "93dba98c050e9abfc623ec66aa499dbbb46b2fe1"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :fetcher github :repo
                      "joaotavora/yasnippet" :files
                      (:defaults ("doc" "doc/*.org")) :source "MELPA"
                      :id yasnippet :type git :protocol https :inherit
                      t :depth treeless :ref
                      "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (zig-mode :source "elpaca-menu-lock-file" :recipe
           (:package "zig-mode" :repo "ziglang/zig-mode" :fetcher
                     codeberg :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id zig-mode :type git :protocol
                     https :inherit t :depth treeless :ref
                     "62bfbaced0222e2bfbc086fa8556adf6b3298476")))
