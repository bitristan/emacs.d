;;; init-evil.el --- Evil configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  :config
  (setq evil-normal-state-tag (string-trim evil-normal-state-tag)
        evil-emacs-state-tag (string-trim evil-emacs-state-tag)
        evil-insert-state-tag (string-trim evil-insert-state-tag)
        evil-visual-char-tag (string-trim evil-visual-char-tag)
        evil-visual-line-tag (string-trim evil-visual-line-tag)
        evil-visual-block-tag (string-trim evil-visual-block-tag)
        evil-visual-screen-line-tag (string-trim evil-visual-screen-line-tag)
        evil-operator-state-tag (string-trim evil-operator-state-tag)))

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
  "bb" (lambda () (interactive) (switch-to-buffer nil))
  "kb" 'kill-buffer-and-window
  "xb" 'consult-buffer)

(my-space-leader-def
  "dd" 'pwd)

(provide 'init-evil)
;;; init-evil.el ends here
