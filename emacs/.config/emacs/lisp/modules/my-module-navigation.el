;;; my-module-navigation.el --- Navigation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; `avy` jump commands and related Evil bindings.

;;; Code:

(use-package avy
  :straight t
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-word-1)
   ("M-g g" . avy-goto-line))
  :config
  (define-key evil-normal-state-map (kbd "s") #'avy-goto-char-timer)
  (define-key evil-normal-state-map (kbd "S") #'avy-goto-word-1)
  (define-key evil-normal-state-map (kbd "gs") #'avy-goto-line)
  (define-key evil-normal-state-map (kbd "gl") #'avy-goto-line)
  (define-key evil-visual-state-map (kbd "s") #'avy-goto-char-timer)
  (setq avy-timeout-seconds 0.3))

(provide 'my-module-navigation)

;;; my-module-navigation.el ends here
