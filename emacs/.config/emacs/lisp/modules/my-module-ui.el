;;; my-module-ui.el --- Themes and UI niceties -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme collections, dashboard, and focused-reading modes.

;;; Code:

(use-package kaolin-themes
  :straight t)

(use-package modus-themes
  :straight t)

(use-package ef-themes
  :straight t)

(use-package tao-theme
  :straight t)

;; (use-package spacious-padding
;;   :straight t
;;   :config
;;   (spacious-padding-mode 1))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-banner-logo-title "Happy Hacking :)"))

(use-package olivetti
  :straight t
  :commands olivetti-mode
  :custom
  (olivetti-body-width 100))

;; Theme Switcher with Live Preview
(defvar my/theme-before-preview nil
  "Store the theme that was active before previewing.")

(defun my/theme-switcher ()
  "Select and preview themes interactively using completion.
Use C-j/C-k to navigate and see live previews, Enter to apply."
  (interactive)
  (let* ((original-theme (car custom-enabled-themes))
         (themes (mapcar #'symbol-name (custom-available-themes)))
         (current (symbol-name (or original-theme 'none))))
    (consult--read
     themes
     :prompt "Theme: "
     :require-match t
     :category 'theme
     :default current
     :lookup (lambda (selected candidates input narrow)
               (when selected
                 (intern selected)))
     :state (lambda (action cand)
              (pcase action
                ('preview
                 (when cand
                   (mapc #'disable-theme custom-enabled-themes)
                   (load-theme (if (symbolp cand) cand (intern cand)) :no-confirm)))
                ('return
                 (when cand
                   (mapc #'disable-theme custom-enabled-themes)
                   (load-theme (if (symbolp cand) cand (intern cand)) :no-confirm)
                   (message "Applied theme: %s" cand)))
                ('exit
                 (unless cand
                   ;; Restore original theme if cancelled
                   (when original-theme
                     (mapc #'disable-theme custom-enabled-themes)
                     (load-theme original-theme :no-confirm)
                     (message "Cancelled - restored theme: %s" original-theme))))))
     :preview-key 'any)))

(provide 'my-module-ui)

;;; my-module-ui.el ends here
