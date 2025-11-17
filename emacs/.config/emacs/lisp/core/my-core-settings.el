;;; my-core-settings.el --- Core settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Frame/UI defaults, macOS tweaks, and other global settings.

;;; Code:

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq frame-title-format nil)

(when (equal system-type 'darwin)
  (setq ns-auto-hide-menu-bar t)
  (setq ns-use-proxy-icon nil)
  (setq initial-frame-alist
        (append
         '((ns-transparent-titlebar . t)
           (ns-appearance . light)
           (vertical-scroll-bars . nil)
           (internal-border-width . 0))
         initial-frame-alist)))

(setq vc-follow-symlinks t
      ring-bell-function 'ignore
      uniquify-buffer-name-style 'post-forward-angle-brackets
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end-double-space nil
      delete-by-moving-to-trash t
      make-backup-files nil
      auto-save-default nil
      set-mark-command-repeat-pop t
      track-eol t
      line-move-visual nil
      warning-minimum-level :error)

(setq-default kill-whole-line t
              indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq custom-safe-themes t)
(setq electric-pair-mode 1)

;; Dynamic font sizing based on monitor setup
(defun my/adjust-font-size ()
  "Set font height based on number of displays."
  (let* ((display-count
          (if (equal system-type 'darwin)
              ;; On macOS, use system_profiler to count displays
              (string-to-number
               (string-trim
                (shell-command-to-string
                 "system_profiler SPDisplaysDataType | grep -c 'Resolution:'")))
            ;; On other systems, use display-monitor-attributes-list
            (length (display-monitor-attributes-list))))
         (height (if (> display-count 1) 140 320)))
    (set-face-attribute 'default nil
                        :family "Cascadia Code"
                        :height height)
    (message "Font height set to %d (%d display%s detected)"
             height display-count (if (= display-count 1) "" "s"))))

;; Set initial font size
(my/adjust-font-size)

;; Auto-adjust when displays change (macOS)
(when (equal system-type 'darwin)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (my/adjust-font-size))))

(provide 'my-core-settings)

;;; my-core-settings.el ends here
