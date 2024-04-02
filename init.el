;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(when (version< emacs-version "27.1")
  (error "Your Emacs is too old -- this config requires v%s or higher" minver))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'.

Don't put large files in `site-lisp' directory, e.g. EAF.
Otherwise the startup will be very slow."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-const)
(require 'init-funcs)
(require 'init-package)
(require 'init-basic)
(require 'init-ui)
(require 'init-edit)
(require 'init-completion)
(require 'init-ibuffer)
(require 'init-dired)
(require 'init-window)
(require 'init-workspace)
(require 'init-misc)
(require 'init-markdown)
(require 'init-reader)
(require 'init-org)
(require 'init-git)
(require 'init-check)
(require 'init-eglot)
(require 'init-projectile)
(require 'init-prog)
(require 'init-evil)
(require 'init-cc)
(require 'init-web-mode)
(require 'init-python)
(require 'init-go)
(require 'init-rust)

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
