;;; init-cc.el --- C/C++ configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :init (setq-default c-basic-offset 4))

(use-package c-ts-mode
  :init (setq c-ts-mode-indent-offset 4))

(add-hook 'makefile-mode-hook (lambda ()
                                (setq indent-tabs-mode t)))

(provide 'init-cc)
;;; init-cc.el ends here
