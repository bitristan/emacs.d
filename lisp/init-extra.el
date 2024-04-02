;;; init-extra.el --- Extra configuration.  -*- lexical-binding: t -*-
;;; Commentary: some helpful command I personal used.
;;; Code:

(defun open-file-from-clipboard ()
  (interactive)
  (let* ((name (cond
                (sys/macp
                 (shell-command-to-string "pbpaste"))
                (t
                 (shell-command-to-string "xclip -selection c -o")))))
    (if (file-exists-p name)
        (find-file name))))

(defun krn-start-page ()
  "Start krn page from kwai app by specified scheme."
  (interactive)
  (let* ((address (read-from-minibuffer "Scheme: "))
         (command (format "adb shell am start -a android.intent.action.VIEW -d '\"%s\"'" address)))
    (if (string-prefix-p "kwai://krn" address)
        (progn
          (shell-command command)
          (message "Success start krn page."))
      (message "Invalid scheme."))))

(provide 'init-extra)
;;; init-extra.el ends here
