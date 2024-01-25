;;; init-basic.el --- Some basic defaults. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq user-full-name "Ting Sun"
      user-mail-address "sunting.bcwl@gmail.com")

;; utf-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

(display-time-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq native-comp-async-report-warnings-errors nil)

(when (boundp 'mac-option-modifier)
  (setq mac-option-modifier 'meta))

(provide 'init-basic)
;;; init-basic.el ends here
