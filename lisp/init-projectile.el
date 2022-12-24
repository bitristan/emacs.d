;;; init-projectile.el --- Integrate projectile.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :hook (after-init . projectile-mode)
  :config
  (setq-default projectile-mode-line-prefix " Proj")
  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package ibuffer-projectile
  :after projectile)

(provide 'init-projectile)
;;; init-projectile.el ends here
