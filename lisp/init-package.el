;;; init-package.el --- Use use-package as package manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

(setq package-archives
      '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("nognu" . "https://mirrors.ustc.edu.cn/elpa/nognu/")
        ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; More options
(setq package-install-upgrade-built-in t)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;; Required by `use-package'
(use-package diminish :ensure t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

(provide 'init-package)
;;; init-package.el ends here
