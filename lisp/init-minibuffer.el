;;; init-minibuffer.el --- Customize minibuffer.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package marginalia
  :after vertico
  :hook (after-init . marginalia-mode))

(use-package embark
  :after vertico)

(use-package consult
  :commands (consult-ripgrep)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark-consult
  :after (embark consult))

(setq tab-always-indent 'complete)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (setq completion-category-defaults nil
        completion-category-overrides nil
        completion-cycle-threshold 4))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  :config
  (setq-default corfu-auto t)
  (setq-default corfu-quit-no-match 'separator)
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))
  (when (featurep 'corfu-popupinfo)
    (with-eval-after-load 'corfu
      (corfu-popupinfo-mode))))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
