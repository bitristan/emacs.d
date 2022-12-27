;;; init-ui.el --- Customize ui.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-themes
  :init
  (load-theme 'doom-dracula t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-buffer-file-name-style 'relative-to-project))

(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '("Fira Code" "Source Code Pro"
                         "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (member font (font-family-list))
           return (set-face-attribute 'default nil :font font :height 110))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
           when (member font (font-family-list))
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Micro Hei" "Source Code Pro" "WenQuanYi Zen Hei")
           when (member font (font-family-list))
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

(provide 'init-ui)
;;; init-ui.el ends here
