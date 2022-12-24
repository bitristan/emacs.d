;;; init-editor.el --- Editor related configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 tab-width 4
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 truncate-lines nil
 truncate-partial-width-windows nil
 fill-column 100
 indicate-buffer-boundaries 'left)

(setq tab-always-indent 'complete)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))

(use-package display-line-numbers
  :hook (after-init . global-display-line-numbers-mode)
  :config
  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  vterm-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(use-package display-fill-column-indicator
  :custom
  (display-fill-column-indicator-character ?\u254e)
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :custom
  (revert-without-query (list "\\.png$" "\\.svg$"))
  (global-auto-revert-non-file-buffers t))

(use-package savehist
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package recentf
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(use-package super-save
  :init
  (super-save-mode +1)
  (add-to-list 'super-save-triggers 'ace-window)
  (setq super-save-auto-save-when-idle t))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package anzu
  :bind (([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap query-replace] . anzu-query-replace)))

;; Highlight symbols
(use-package symbol-overlay
  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :bind (("M-i" . symbol-overlay-put)
         ("M-I" . symbol-overlay-remove-all)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward))
  :hook (((prog-mode yaml-mode) . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1)
  :config
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode 'yaml-mode)
      (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))

;; Highlight the current line
(use-package hl-line
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package avy)
(use-package wgrep)
(use-package sudo-edit)
(use-package yasnippet
  :hook (after-init . yas-global-mode))
(use-package yasnippet-snippets
  :after yasnippet)
(use-package auto-yasnippet)
(use-package expand-region)

(provide 'init-editor)
;;; init-editor.el ends here
