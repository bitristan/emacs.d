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

(provide 'init-evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-evil.el ends here
