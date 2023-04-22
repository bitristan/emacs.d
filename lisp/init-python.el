;;; init-python.el --- Python config.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python-mode
  :hook (python-mode . anaconda-mode))
(use-package anaconda-mode)

(provide 'init-python)
;;; init-python.el ends here
