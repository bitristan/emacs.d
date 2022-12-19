;;; early-init.el --- Emacs 27+ pre-initialisation config
;;; Commentary:
;;; Code:

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-splash-screen t
      use-file-dialog nil
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-line-close-button-show nil
      comp-deferred-compilation nil)

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
