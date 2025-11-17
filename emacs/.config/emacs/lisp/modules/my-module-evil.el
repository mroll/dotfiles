;;; my-module-evil.el --- Evil setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Core Evil packages and related helpers.

;;; Code:

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :straight t
  :after evil
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "kj"))

(provide 'my-module-evil)

;;; my-module-evil.el ends here
