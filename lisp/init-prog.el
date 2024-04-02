;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

;; Tree-sitter support
(use-package treesit-auto
  :defines treesit-auto-install
  :hook (after-init . global-treesit-auto-mode)
  :init (setq treesit-auto-install 'prompt))

;; Code styles
(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

;; Cross-referencing commands
(use-package xref
  :commands xref-show-definitions-completing-read
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

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
(use-package dart-mode)
(use-package swift-mode)
(use-package vimrc-mode)
(use-package solidity-mode)
(use-package clojure-mode)
(use-package clojure-snippets
  :after clojure-mode)

(use-package lua-mode
  :custom
  (lua-indent-level 2)
  (lua-indent-nested-block-content-align nil)
  (lua-indent-close-paren-align nil)
  (lua-indent-string-contents t))

(use-package dart-mode)
(use-package soong-mode)

(use-package antlr-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode)))

(provide 'init-prog)
;;; init-programming.el ends here
