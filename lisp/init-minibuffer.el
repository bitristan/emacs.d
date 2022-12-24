;;; init-minibuffer.el --- Customize minibuffer.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package marginalia
  :after vertico
  :hook (after-init . marginalia-mode))

(use-package embark
  :bind (("M-RET"   . embark-either)
         :map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-c" . embark-export))
  :config
  (defun embark-either (&optional arg)
    "Invoke `embark-act' is ARG is non-nil, otherwise invoke
`embark-dwim'."
    (interactive "P")
    (if arg
        (embark-act)
      (embark-dwim))))

(use-package consult
  :bind (([remap imenu]              . consult-imenu)
         ([remap goto-line]          . consult-goto-line)
         ([remap bookmark-jump]      . consult-bookmark)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap evil-show-marks]    . consult-mark)
         ([remap switch-to-buffer]   . consult-buffer)
         ("C-s" . consult-line))
  :init
  (with-eval-after-load 'evil
    (evil-global-set-key 'motion "gm" #'consult-imenu)
    (evil-global-set-key 'motion "go" #'consult-outline))
  :config
  (consult-customize
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult--source-bookmark consult--source-recent-file
    consult--source-project-recent-file
    :preview-key (kbd "M-."))
  :custom
  (defun get-project-root ()
    (when (fboundp 'projectile-project-root)
      (projectile-project-root)))
  (consult-project-root-function #'get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(use-package embark-consult
  :after (embark consult))

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
