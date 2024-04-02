;;; init-org.el --- Org configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst me-org-dir "~/workspace/github/tinker-s/todo/org"
  "Directory of org files.")

(use-package org
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-directory me-org-dir)
  (setq org-startup-indented t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-reschedule 'time)
  (setq org-log-redeadline 'time)
  (setq org-log-into-drawer t)
  (setq org-capture-templates
        `(("i" "Idea" entry (file ,(concat org-directory "/idea.org"))
           "*  %^{Title} %?\n%U\n%a\n")
          ("t" "Todo" entry (file ,(concat org-directory "/gtd.org"))
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "Note" entry (file ,(concat org-directory "/note.org"))
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+olp+datetree
                                ,(concat org-directory "/journal.org"))
           "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("b" "Book" entry (file+olp+datetree
                             ,(concat org-directory "/book.org"))
	       "* Topic: %^{Description}  %^g %? Added: %U")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)")))
  (setq org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning)))
  (setq org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success)))
  (setq org-agenda-files (list me-org-dir))
  (setq org-agenda-block-separator ?─)
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

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defconst load-language-alist
    '((emacs-lisp . t)
      (perl       . t)
      (python     . t)
      (ruby       . t)
      (js         . t)
      (css        . t)
      (sass       . t)
      (C          . t)
      (java       . t)
      (shell      . t)
      (plantuml   . t))
    "Alist of org ob languages.")

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-alist))

  (use-package ob-powershell
    :init (cl-pushnew '(powershell . t) load-language-alist))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-alist))

  ;; Install: npm install -g @mermaid-js/mermaid-cli
  (use-package ob-mermaid
    :init (cl-pushnew '(mermaid . t) load-language-alist))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-alist)

  ;; Add md/gfm backends
  (add-to-list 'org-export-backends 'md)
  (use-package ox-gfm
    :init (add-to-list 'org-export-backends 'gfm)))

;; Prettify UI
(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)
         (org-modern-mode . (lambda ()
                              "Adapt `org-modern-mode'."
                              ;; Disable Prettify Symbols mode
                              (setq prettify-symbols-alist nil)
                              (prettify-symbols-mode -1)))))
;; Table of contents
(use-package toc-org
  :after org
  :hook (org-mode . toc-org-mode))

;; Add graphical view of agenda
(use-package org-timeline
  :after org
  :hook (org-agenda-finalize . org-timeline-insert-timeline))

;; Preview
(use-package org-preview-html
  :after org
  :diminish
  :init (when (featurep 'xwidget-internal)
          (setq org-preview-html-viewer 'xwidget)))

;; Presentation
(use-package org-tree-slide
  :after org
  :diminish
  :functions (org-display-inline-images
              org-remove-inline-images)
  :hook ((org-tree-slide-play . (lambda ()
                                  (text-scale-increase 4)
                                  (org-display-inline-images)
                                  (read-only-mode 1)))
         (org-tree-slide-stop . (lambda ()
                                  (text-scale-increase 0)
                                  (org-remove-inline-images)
                                  (read-only-mode -1))))
  :init (setq org-tree-slide-header nil
              org-tree-slide-slide-in-effect t
              org-tree-slide-heading-emphasis nil
              org-tree-slide-cursor-init t
              org-tree-slide-modeline-display 'outside
              org-tree-slide-skip-done nil
              org-tree-slide-skip-comments t
              org-tree-slide-skip-outline-level 3))

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

;; Roam
(use-package org-roam
  :diminish
  :defines org-roam-graph-viewer
  :init
  (setq org-roam-directory (file-truename me-org-dir)
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        org-roam-graph-viewer (if (featurep 'xwidget-internal)
                                  #'xwidget-webkit-browse-url
                                #'browse-url))
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))
  (add-to-list 'org-agenda-files (format "%s/%s" org-roam-directory "roam"))

  (org-roam-db-autosync-enable))

(use-package org-roam-ui
  :init (when (featurep 'xwidget-internal)
          (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url)))

(provide 'init-org)
;;; init-org.el ends here
