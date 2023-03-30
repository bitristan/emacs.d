;;; init-programming.el --- Programming configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tree-sitter
  :hook ((go-mode . tree-sitter-hl-mode)
         (rust-mode . tree-sitter-hl-mode)))
(use-package tree-sitter-langs
  :after tree-sitter)

(use-package apheleia
  :defines (apheleia-formatters)
  :commands (apheleia-global-mode)
  :init
  (apheleia-global-mode 1)
  :config
  (add-to-list 'apheleia-formatters
               '(prettier . (npx "prettier" "--stdin-filepath" filepath "--tab-width=4"))))

(use-package php-mode)
(use-package markdown-mode)
(use-package groovy-mode)
(use-package kotlin-mode)
(use-package cmake-mode)
(use-package protobuf-mode)
(use-package toml-mode)
(use-package yaml-mode
  :hook ((yaml-mode . whitespace-mode)
         (yaml-mode . subword-mode)))
(use-package haml-mode)
(use-package cask-mode)
(use-package csharp-mode)
(use-package julia-mode)
(use-package haskell-mode)
(use-package powershell)
(use-package scala-mode)
(use-package swift-mode)
(use-package vimrc-mode)
(use-package solidity-mode)
(use-package clojure-mode)
(use-package clojure-snippets
  :after clojure-mode)
(use-package fish-mode
  :hook (fish-mode . (lambda ()
                       (add-hook 'before-save-hook
                                 #'fish_indent-before-save))))
(use-package lua-mode
  :custom
  (lua-indent-level 2)
  (lua-indent-nested-block-content-align nil)
  (lua-indent-close-paren-align nil)
  (lua-indent-string-contents t))

(provide 'init-programming)
;;; init-programming.el ends here
