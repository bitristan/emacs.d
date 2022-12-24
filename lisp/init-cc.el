;;; init-cc.el --- C/C++ configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'c-mode-common-hook (lambda ()
                                (setq c-basic-offset 4)))

(add-hook 'makefile-mode-hook (lambda ()
                                (setq indent-tabs-mode t)))

(provide 'init-cc)
;;; init-cc.el ends here
