;;; init-eglot.el --- Eglot as lsp client.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :hook ((go-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown nil))

(use-package consult-eglot
  :after eglot)

(provide 'init-eglot)
;;; init-eglot.el ends here
