;;; init-misc.el --- Some other useful tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package command-log-mode)
(use-package scratch)
(use-package restart-emacs)
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

(use-package treemacs)
(use-package pandoc-mode
  :hook (markdown-mode . pandoc-mode))

(use-package youdao-dictionary
  :config
  (setq youdao-dictionary-search-history-file (concat user-emacs-directory ".youdao"))
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'youdao-dictionary-mode-map "q" #'quit-window)))

(use-package keyfreq
  :hook ((after-init . keyfreq-mode)
         (keyfreq-mode . keyfreq-autosave-mode))
  :init
  (setq keyfreq-excluded-commands
        '(self-insert-command
          evil-previous-line
          evil-next-line
          evil-jump-item
          evil-backward-char
          evil-scroll-page-up
          evil-scroll-page-down
          ivy-backward-delete-char
          ivy-backward-kill-word
          ivy-done
          ivy-next-line
          ivy-occur
          ivy-occur-next-line
          ivy-occur-press-and-switch
          ivy-occur-previous-line
          ivy-previous-line
          ivy-wgrep-change-to-wgrep-mode)))

(use-package tramp
  :init
  (setq tramp-default-method "ssh"))

(provide 'init-misc)
;;; init-misc.el ends here
