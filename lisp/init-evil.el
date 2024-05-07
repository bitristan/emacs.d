;; init-evil.el --- Use evil for more effective editing.	-*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 bitristan

;; Author: bitristan <sunting.bcwl@gmail.com>
;; URL: https://github.com/bitristan/emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; More effective editing by using evil.
;;

;;; Code:

(use-package evil
  :hook (after-init . evil-mode)
  :init
  ;; (setq evil-undo-system 'vundo)
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-keybinding nil))

(use-package evil-escape
  :after evil
  :hook (evil-mode . evil-escape-mode))

(use-package evil-surround
  :after evil
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-matchit
  :after evil
  :custom
  (evilmi-shortcut "m")
  :hook (evil-mode . global-evil-matchit-mode))

(use-package evil-collection
  :hook
  (evil-mode . evil-collection-init))

(use-package evil-commentary
  :hook
  (evil-mode . evil-commentary-mode))

(use-package evil-lion
  :custom
  (evil-lion-squeeze-spaces nil)
  :hook
  (evil-mode . evil-lion-mode))

(use-package general
  :commands
  (my-comma-leader-def)
  :config
  (general-create-definer my-comma-leader-def
    :prefix ","
    :states '(normal visual)))

(my-comma-leader-def
  "bf" 'beginning-of-defun
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
  "gs" 'magit-status
  "ee" 'eval-expression
  "pf" 'project-find-file
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
  "xm" 'execute-extended-command)

(provide 'init-evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-evil.el ends here
