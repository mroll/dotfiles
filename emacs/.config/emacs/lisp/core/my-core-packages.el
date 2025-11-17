;;; my-core-packages.el --- Package/bootstrap helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; straight.el bootstrap and general helper commands used across modules.

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'org)

;; Helper functions used throughout the config
(defun my/dired-this-buffer ()
  "Open the current buffer's directory in Dired."
  (interactive)
  (dired (file-name-directory buffer-file-name)))

(defun compile--make ()
  "Invoke `compile' with \"make\"."
  (interactive)
  (compile "make"))

(defun switch-to-last-buffer ()
  "Toggle back to the previously active buffer."
  (interactive)
  (switch-to-buffer nil))

(defun reload-config ()
  "Reload init.el and all modules."
  (interactive)
  ;; Unload all custom modules so they can be reloaded
  (when (boundp 'my/emacs-modules)
    (dolist (feature my/emacs-modules)
      (unload-feature feature t)))
  ;; Now reload init.el which will reload all modules
  (load-file "~/.config/emacs/init.el")
  (message "Configuration reloaded!"))

(defun open-config-file ()
  "Open init.el for editing."
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(defun open-line-above ()
  "Insert an empty line above the current one and move to it."
  (interactive)
  (beginning-of-line)
  (open-line 1))

(defun my/org-insert-inactive-timestamp-now ()
  "Insert an inactive Org timestamp with minute precision."
  (interactive)
  (require 'org)
  (let ((org-time-stamp-rounding-minutes '(0 1)))
    (org-insert-time-stamp (current-time) t t)))

(provide 'my-core-packages)

;;; my-core-packages.el ends here
