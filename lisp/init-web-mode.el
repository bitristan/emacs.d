;;; init-web-mode.el --- Web development configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun setup-tide-mode ()
  (tide-setup)
  (tide-hl-identifier-mode 1))

(use-package web-mode
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.tsx\\'" . web-mode)
   ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-block-padding 4
        web-mode-comment-style 4
        web-mode-enable-auto-quoting nil
        web-mode-enable-current-element-highlight t)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package json-mode
  :init
  (setq json-reformat:indent-width 4))

(use-package css-mode
  :init (setq css-indent-offset 4))

(use-package scss-mode
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

(use-package js2-mode
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :init (setq js-indent-level 4)
  :config
  (with-eval-after-load 'flycheck
    (when (or (executable-find "eslint_d")
              (executable-find "eslint")
              (executable-find "jshint"))
      (setq js2-mode-show-strict-warnings nil))
    (when (executable-find "eslint_d")
      ;; https://github.com/mantoni/eslint_d.js
      ;; npm -i -g eslint_d
      (setq flycheck-javascript-eslint-executable "eslint_d")))

  (use-package js2-refactor
    :diminish
    :hook (js2-mode . js2-refactor-mode)))

(use-package typescript-mode
  :config
  (setq typescript-indent-level 4)
  (add-hook 'typescript-mode #'subword-mode))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . (lambda () (setup-tide-mode)))))

(use-package add-node-modules-path
  :hook ((typescript-mode js-mode js2-mode web-mode) . add-node-modules-path))

(provide 'init-web-mode)
;;; init-web-mode.el ends here
