;; -*- mode: emacs-lisp; indent-tabs-mode: nil; lexical-binding: t -*-

(elpaca ac-rtags)
(elpaca adaptive-wrap)
(elpaca apheleia)
(elpaca auto-complete)
(elpaca base16-theme)
(elpaca bison-mode)
(elpaca caddyfile-mode)
(elpaca chatgpt-shell)
(elpaca clang-format)

(elpaca clipetty
  (global-clipetty-mode 1))

(elpaca clojure-mode)
;; (elpaca cmake-mode)
(elpaca coffee-mode
  (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
  (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode)))

(elpaca company
  (setopt company-tooltip-limit 20
          company-minimum-prefix-length 3
          company-idle-delay .3
          company-echo-delay 0
          company-begin-commands '(self-insert-command)
          company-dabbrev-downcase nil)
  (global-company-mode 1))

(elpaca company-coq)
(elpaca company-go)
(elpaca company-rtags)
(elpaca conda)
(elpaca counsel
  (global-set-key (kbd "M-x") #'counsel-M-x)
  (global-set-key (kbd "M-y") #'my-yank-pop)
  (global-set-key (kbd "C-M-y") #'counsel-yank-pop)
  (global-set-key (kbd "C-x C-f") 'my-find-file))

(elpaca csv)
(elpaca csv-mode)
(elpaca cuda-mode)
(elpaca dash-at-point)
;; (elpaca debian-changelog-mode)
;; (elpaca docker-compose-mode)
(elpaca dockerfile-mode)

;; (elpaca edit-server
;;   (require 'edit-server)
;;   (edit-server-start))

(elpaca ein)
(elpaca eldev)
(elpaca elixir-mode)
(elpaca envrc)
;; (elpaca erlang)
(elpaca exec-path-from-shell)
(elpaca f)
(elpaca flycheck)
(elpaca flycheck-aspell)
(elpaca flycheck-clangcheck)
(elpaca flycheck-elixir)
(elpaca flycheck-julia)
(elpaca flycheck-ocaml)
(elpaca flycheck-rust)
(elpaca fuzzy)
(elpaca git-link)
(elpaca gnu-elpa-keyring-update)
(elpaca gnuplot)
(elpaca gnuplot-mode)
(elpaca gptel)
(elpaca graphviz-dot-mode)
(elpaca haskell-mode)
(elpaca helm)
;; (elpaca helm-git-files)
(elpaca helm-ls-git)
(elpaca inf-ruby)
(elpaca isortify)

(elpaca ivy
  (ivy-mode 1)
  ;; ;; number of result lines to display
  ;; (setq ivy-height 10)
  ;; ;; does not count candidates
  ;; (setq ivy-count-format "")
  ;; ;; no regexp by default
  ;; (setq ivy-initial-inputs-alist nil)
  ;; ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order))

        counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)")))

(elpaca javaimp)
(elpaca jinja2-mode)
(elpaca js2-mode)
(elpaca jsonian)
(elpaca julia-mode)
(elpaca julia-repl)

(elpaca kkp
  (global-kkp-mode 1))

(elpaca kotlin-ts-mode)
(elpaca ledger-mode)

(elpaca (lean4-mode
         :type git
         :host github
         :repo "leanprover-community/lean4-mode"))

(elpaca lsp-mode)

(elpaca lsp-pyright
  (require 'lsp-pyright))

(elpaca lsp-python-ms)
(elpaca lsp-ui)
(elpaca lua-mode)
(elpaca magit-gerrit)
(elpaca magit-gh-pulls)
(elpaca magit-popup)
(elpaca mistty)

(elpaca mode-line-bell
  (mode-line-bell-mode 1))

(elpaca ninja-mode)
(elpaca nix-mode)
(elpaca polymode)

(elpaca
    `(obsidian
      :type file
      :main ,(expand-file-name "~/.emacs.d/home-manager/obsidian.el"))
  (require 'obsidian))

(elpaca popwin
  (require 'popwin)
  (let ((tmp nil))
  (mapc (lambda (e)
          (if (not (or (and (consp e) (or (eq (car e) 'grep-mode)
                                          (eq (car e) 'occur-mode)
                                          (eq (car e) 'compilation-mode)))
                       (equal e "*vc-diff*")))
              (setq tmp (cons e tmp))))
        popwin:special-display-config)
  (setq popwin:special-display-config tmp)))

(elpaca prettier-js)
(elpaca protobuf-mode)
(elpaca py-isort)
(elpaca python-black)
(elpaca reformatter)
(elpaca rubocop)
(elpaca ruby-electric)
;; (elpaca ruby-mode)
;; (elpaca ruby-tools)
(elpaca scala-mode)
(elpaca seq)
(elpaca svelte-mode)
(elpaca swiper)
(elpaca terraform-mode)
(elpaca typst-ts-mode)
(elpaca verilog-mode)
(elpaca visual-fill-column)
(elpaca vterm)
(elpaca web-mode)
(elpaca window-number
  (require 'window-number)
  (window-number-define-keys window-number-mode-map "C-c ")
  (window-number-mode 1))
(elpaca yaml)
(elpaca yaml-mode)
(elpaca zig-mode)
