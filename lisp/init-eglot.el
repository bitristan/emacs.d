;; init-eglot.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :init
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5)
  :config
  (use-package consult-eglot))

;; Enable LSP in org babel
;; https://github.com/emacs-lsp/lsp-mode/issues/377
(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (cl-check-type lang string)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (setq buffer-file-name (or (->> info caddr (alist-get :file))
                                    "org-src-babel.tmp"))
         (when (fboundp 'eglot-ensure)
           (eglot-ensure)))
       (put ',intern-pre 'function-documentation
            (format "Enable eglot in the buffer of org source block (%s)." (upcase ,lang)))

       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))

(defconst org-babel-lang-list
  '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++" "shell")
  "The supported programming languages for interactive Babel.")
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(when emacs/>=29p
  (use-package dape
    :custom (dape-buffer-window-arrangment 'right)
    :config
    ;; Save buffers on startup, useful for interpreted languages
    (add-hook 'dape-on-start-hooks
              (defun dape--save-on-start ()
                (save-some-buffers t t)))))

(provide 'init-eglot)
;;; init-eglot.el ends here
