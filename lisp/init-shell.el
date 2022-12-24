;;; init-shell.el --- Shell related configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

(provide 'init-shell)
;;; init-shell.el ends here
