;;; my-lang-lsp.el --- LSP/tooling config -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared company/flycheck/lsp setup used by language modules.

;;; Code:

(use-package company
  :straight t
  :diminish company-mode
  :init
  (global-company-mode 1)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (company-backends '((company-capf))))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 0.5))

(use-package lsp-mode
  :straight t
  :hook ((c-mode c++-mode python-mode) . lsp)
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l"))

(defun my-lsp-format-on-save ()
  "Add LSP formatting to the current buffer."
  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

(add-hook 'lsp-mode-hook #'my-lsp-format-on-save)

(provide 'my-lang-lsp)

;;; my-lang-lsp.el ends here
