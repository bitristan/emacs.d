;;; init-rust.el --- Rust config.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rustic
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save t))

(provide 'init-rust)
;;; init-rust.el ends here
