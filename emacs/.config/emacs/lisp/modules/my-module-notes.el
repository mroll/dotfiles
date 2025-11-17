;;; my-module-notes.el --- Notes & bibliography -*- lexical-binding: t; -*-

;;; Commentary:
;; Citar, Deft, PDF tools, and other writing helpers.

;;; Code:

(use-package citar
  :straight t
  :config
  (setq citar-bibliography '("~/notes/hft/hft.bib")
        citar-library-paths '("~/notes/hft/pdfs/")
        citar-notes-paths '("~/notes/hft/")
        citar-file-open-functions '(("pdf" . citar-file-open-external))
        citar-at-point-function 'embark-act))

(use-package citar-org-roam
  :straight t
  :after (citar org-roam)
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-subdir "sources/"))

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq pdf-view-display-size 'fit-page
        pdf-view-resize-factor 1.1))

;; (use-package denote
;;   :straight t
;;   :hook (dired-mode . denote-dired-mode)
;;   :config
;;   (setq denote-directory (expand-file-name "~/Documents/notes/"))
;;   (denote-rename-buffer-mode 1))

(use-package deft
  :straight t
  :config
  (setq deft-directory "~/Documents/notes/"
        deft-recursive t
        deft-auto-save-interval nil))

;; (use-package consult-notes
;;   :straight t
;;   :after denote
;;   :config
;;   (setq consult-notes-sources
;;         `(("Notes" ?n ,denote-directory)))
;;   (consult-notes-denote-mode))

(provide 'my-module-notes)

;;; my-module-notes.el ends here
