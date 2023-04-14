;;; init-org.el --- Org configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst my-org-dir "~/workspace/github/tinker-s/todo/org"
  "Directory of org files.")

(use-package org
  :config
  (visual-line-mode 1)

  (setq org-indent-mode t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-reschedule 'time)
  (setq org-log-redeadline 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")) ; Task was completed
        )
  (setq org-agenda-files
        `(,(concat (file-name-as-directory my-org-dir) "work.org")
	  ,(concat (file-name-as-directory my-org-dir) "family.org")
	  ,(concat (file-name-as-directory my-org-dir) "coding.org")))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ;; Re-align tags when window shape changes
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t)))
  (setq org-capture-templates
        `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
           "* NEXT %?\n%U\n" :clock-resume t)
          ("n" "note" entry (file "")
           "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
          ))

 (org-babel-do-load-languages
   'org-babel-load-languages
   `((emacs-lisp . t)
     (perl . t)
     (python . t)
     (ruby . t)
     (js . t)
     (css . t)
     (sass . t)
     (C . t)
     (java . t)
     (plantuml . t))))

(use-package evil-org
  :commands (evil-org-agenda-set-keys)
  :after (org evil)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package valign
  :config
  (add-hook 'org-mode-hook #'valign-mode))

(use-package org-present
  :commands
  (org-present-big
   org-display-inline-images
   org-present-hide-cursor
   org-present-read-only
   org-present-small
   org-remove-inline-images
   org-present-show-cursor
   org-present-read-write)
  :init
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

(use-package org-bullets
  :hook ((org-mode . org-bullets-mode)))

(provide 'init-org)
;;; init-org.el ends here
