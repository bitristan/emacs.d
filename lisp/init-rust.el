;;; init-rust.el --- Rust config.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rustic
  :init
  (setq rustic-lsp-client 'eglot))

(provide 'init-rust)
;;; init-rust.el ends here
