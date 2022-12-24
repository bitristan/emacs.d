;;; init-git.el --- Git integrate.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package git-timemachine)
(use-package magit)
(use-package git-gutter
  :hook (after-init . global-git-gutter-mode)
  :custom (git-gutter:disabled-modes '(image-mode)))

(provide 'init-git)
;;; init-git.el ends here
