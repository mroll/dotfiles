;;; my-lang-python.el --- Python support -*- lexical-binding: t; -*-

;;; Commentary:
;; Python major mode and virtualenv helpers.

;;; Code:

(use-package python-mode
  :straight t
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent-offset 4))

(use-package pyvenv
  :straight t
  :hook (python-mode . pyvenv-mode)
  :custom
  (pyvenv-default-virtual-env-name nil))

(provide 'my-lang-python)

;;; my-lang-python.el ends here
