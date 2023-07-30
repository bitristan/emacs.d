;;; init-evil.el --- Evil configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-undo-system 'undo-fu)
  :config
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line))

(use-package evil-escape
  :after evil
  :hook (evil-mode . evil-escape-mode))

(use-package evil-surround
  :after evil
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-matchit
  :after evil
  :defines evilmi-shortcut
  :init (setq evilmi-shortcut "m")
  :hook (evil-mode . global-evil-matchit-mode))

(use-package evil-collection
  :after evil
  :demand
  :init
  (setq evil-collection-magit-use-z-for-folds nil)
  :config
  (evil-collection-init))

(use-package general
  :demand t
  :config
  (general-create-definer my-comma-leader-def
                          :prefix ","
                          :states '(normal visual))
  (general-create-definer my-space-leader-def
                          :prefix "SPC"
                          :states '(normal visual)))

(my-comma-leader-def
  "bf" 'beginning-of-defun
  "0" 'winum-select-window-0-or-10
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9
  "xm" 'execute-extended-command
  "yi" 'youdao-dictionary-search-from-input
  "rr" 'consult-recent-file
  "gg" 'consult-git-grep
  "yy" 'consult-yank-from-kill-ring
  "bb" (lambda () (interactive) (switch-to-buffer nil))
  "kb" 'kill-buffer-and-window
  "xb" 'consult-buffer
  "xk" 'kill-buffer
  "xx" 'er/expand-region
  "xs" 'save-buffer
  "xf" 'find-file
  "ff" 'toggle-full-window
  "fc" 'open-file-from-clipboard
  "gs" 'magit-status
  "ee" 'eval-expression
  "tt" 'treemacs
  "xc" 'vterm-toggle)

(my-space-leader-def
  "dd" 'pwd)

(provide 'init-evil)
;;; init-evil.el ends here
