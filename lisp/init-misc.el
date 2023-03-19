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

(setq my-qrencode-dir (concat (file-name-as-directory "~/.emacs.d/site-lisp") "qrencode"))
(use-package qrencode
  :load-path my-qrencode-dir
  :init
  (autoload 'qrencode-url-at-point "qrencode" "" t)
  (autoload 'qrencode-region "qrencode" "" t)
  (autoload 'qrencode-from-input "qrencode" "" t)
  :config
  (defun qrencode-from-input ()
    "Encode fro input."
    (interactive)
    (let ((text (read-from-minibuffer "Text to encode: ")))
      (if (null text)
          (message "No text found.")
        (qrencode--encode-to-buffer text)))))

(provide 'init-misc)
;;; init-misc.el ends here
