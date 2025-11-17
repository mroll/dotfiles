;;; my-lang-web.el --- Web/markup modes -*- lexical-binding: t; -*-

;;; Commentary:
;; Astro/HTML editing helpers.

;;; Code:

(use-package web-mode
  :straight t
  :mode ("\\.astro\\'" . web-mode)
  :custom
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-quoting t)
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2))

(provide 'my-lang-web)

;;; my-lang-web.el ends here
