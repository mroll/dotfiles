;;; my-lang-tex.el --- LaTeX support -*- lexical-binding: t; -*-

;;; Commentary:
;; AUCTeX defaults for writing papers/notes.

;;; Code:

(use-package tex
  :straight auctex
  :defer t
  :init
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-command-default "LatexMk")
  (setq-default TeX-master nil))

(provide 'my-lang-tex)

;;; my-lang-tex.el ends here
