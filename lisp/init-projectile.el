;;; init-projectile.el --- Integrate projectile.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :hook (after-init . projectile-mode)
  :config
  (setq-default projectile-mode-line-prefix " Proj")
  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ibuffer-projectile
  :after projectile)

(provide 'init-projectile)
;;; init-projectile.el ends here
