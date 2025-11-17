;;; my-core-editing.el --- Core editing packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Foundational packages and hooks that aren't tied to a specific feature module.

;;; Code:

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package recentf
  :straight t
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 20000000)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     "cache"
                     "recentf"
                     "COMMIT_EDITMSG\\'"))
  :preface
  (defun recentf-save-list-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if (fboundp 'shut-up)
          (shut-up (recentf-save-list))
        (recentf-save-list)))
    (message ""))

  (defun recentf-cleanup-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if shutup-p
          (shut-up (recentf-cleanup))
        (recentf-cleanup)))
    (message ""))
  :hook
  (focus-out-hook . (recentf-save-list-silence recentf-cleanup-silence)))

(use-package hl-line
  :straight t
  :hook
  (after-init . global-hl-line-mode))

(use-package restart-emacs
  :straight t)

(use-package whitespace
  :straight t
  :hook (before-save . whitespace-cleanup))

;; Persist history over Emacs restarts.
(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; Auto-save files after idle time
(use-package emacs
  :custom
  (auto-save-visited-interval 2)  ; seconds of idle time before auto-saving
  :config
  (auto-save-visited-mode 1))

(provide 'my-core-editing)

;;; my-core-editing.el ends here
