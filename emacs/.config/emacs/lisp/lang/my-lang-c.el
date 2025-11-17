;;; my-lang-c.el --- C/cc configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; cc-mode style tweaks for C projects.

;;; Code:

(use-package cc-mode
  :ensure nil
  :hook
  (c-mode . my-c-mode-setup)
  :config
  (defun my-c-mode-setup ()
    (c-set-style "linux")
    (setq c-basic-offset 4
          indent-tabs-mode nil
          tab-width 4)
    (message "c-mode customizations loaded")))

(provide 'my-lang-c)

;;; my-lang-c.el ends here
