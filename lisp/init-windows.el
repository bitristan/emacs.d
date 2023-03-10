;;; init-windows.el --- Window related configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package winner
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

(use-package winum
  :hook (after-init . winum-mode))

(use-package ace-window
  :init
  (global-set-key [remap other-window] 'ace-window))

(defun toggle-full-window()
  "Toggle full view of selected window."
  (interactive)
  ;; @see http://www.gnu.org/software/emacs/manual/html_node/elisp/Splitting-Windows.html
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))

(provide 'init-windows)
;;; init-windows.el ends here
