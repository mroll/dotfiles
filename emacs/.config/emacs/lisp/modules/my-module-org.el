;;; my-module-org.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Core Org setup, agenda tweaks, and supporting packages.

;;; Code:

(use-package org-super-agenda
  :straight t
  :config
  (org-super-agenda-mode))

(use-package org-fancy-priorities
  :straight t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-priority-highest 0
        org-priority-default 2
        org-priority-lowest 4)
  (setq org-fancy-priorities-list
        '((?0 . "P0")
          (?1 . "P1")
          (?2 . "P2")
          (?3 . "P3")
          (?4 . "P4"))
        org-priority-faces
        '((?0 :foreground "DarkRed" :background "LightPink")
          (?1 :foreground "DarkOrange4" :background "LightGoldenrod")
          (?2 :foreground "gray20" :background "gray")
          (?3 :foreground "gray20" :background "gray")
          (?4 :foreground "gray20" :background "gray"))))

(use-package org
  :straight t
  :config
  (require 'org-tempo)
  (setq mroll/org-directory "~/org/"
        mroll/org-agenda-directory mroll/org-directory
        org-hide-emphasis-markers t
        org-use-speed-commands t
        org-directory mroll/org-directory
        org-default-notes-file (concat mroll/org-directory "inbox.org")
        org-agenda-window-setup 'only-window
        org-clock-in-resume t
        org-clock-out-when-done t
        org-clock-persist t
        org-clock-persist-query-resume nil
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        org-clock-report-include-clocking-task t
        org-clock-idle-time 5
        org-clock-out-remove-zero-time-clocks t
        org-enforce-todo-dependencies t
        org-hide-leading-stars t
        org-startup-indented nil
        org-cycle-separator-lines 0
        org-insert-heading-respect-content t
        org-log-into-drawer t
        org-log-state-notes-insert-after-drawers nil
        org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))
        org-todo-keyword-faces '(("IN PROGRESS" . "green3"))
        org-refile-targets '((nil :maxlevel . 4)
                             (org-agenda-files :maxlevel . 4))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil)

  (org-indent-mode -1))

(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t))

(use-package org-ql
  :straight t)

(use-package helm-org-ql
  :straight t)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (ocaml . t)
     (shell . t)
     (emacs-lisp . t)))
  (setq org-confirm-babel-evaluate
        (lambda (lang _body)
          (not (member lang '("python" "ocaml" "shell" "emacs-lisp"))))
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-preview-latex-default-process 'dvipng)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append))

(use-package evil-org
  :straight t
  :after (org evil)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(textobjects insert additional shift todo heading))
  (evil-org-agenda-set-keys))

(provide 'my-module-org)

;;; my-module-org.el ends here
