;;; my-module-completion.el --- Completion stack -*- lexical-binding: t; -*-

;;; Commentary:
;; Consult/Vertico stack, Orderless matching, Embark actions, etc.

;;; Code:

(use-package consult
  :straight t
  :config
  (setq consult-project-root-function #'projectile-project-root))

(use-package vertico
  :straight t
  :custom
  (vertico-cycle t)
  (vertico-count 20)
  :init
  (vertico-mode))

(use-package vertico-posframe
  :straight t
  :after vertico
  :config
  (vertico-posframe-mode 1))

;; Emacs minibuffer configurations.
(use-package emacs
  :straight t
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic)))

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'my-module-completion)

;;; my-module-completion.el ends here
