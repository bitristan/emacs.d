;;; init-misc.el --- Some other useful tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Yet another snippet extension
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

;; Collection of yasnippet snippets
(use-package yasnippet-snippets)

;; Yasnippet Completion At Point Function
(use-package yasnippet-capf
  :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package command-log-mode)
(use-package restart-emacs)

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

(use-package youdao-dictionary
  :config
  (setq youdao-dictionary-search-history-file (concat user-emacs-directory ".youdao"))
  (setq youdao-dictionary-app-key (getenv "YOUDAO_APP_KEY"))
  (setq youdao-dictionary-secret-key (getenv "YOUDAO_SECRET_KEY"))
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'youdao-dictionary-mode-map "q" #'quit-window)))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

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

;; Persistent the scratch buffer
(use-package persistent-scratch
  :diminish
  :bind (:map persistent-scratch-mode-map
              ([remap kill-buffer] . (lambda (&rest _)
                                       (interactive)
                                       (user-error "Scratch buffer cannot be killed")))
              ([remap revert-buffer] . persistent-scratch-restore)
              ([remap revert-this-buffer] . persistent-scratch-restore))
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode))
  :init (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
              persistent-scratch-backup-directory
              (expand-file-name "persistent-scratch" user-emacs-directory)))

;; Search tools
;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Fast search tool `ripgrep'
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
              ("c" . rg-dwim-current-dir)
              ("f" . rg-dwim-current-file)
              ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases))

(use-package tramp
  :init
  (setq tramp-default-method "ssh"))

(provide 'init-misc)
;;; init-misc.el ends here
