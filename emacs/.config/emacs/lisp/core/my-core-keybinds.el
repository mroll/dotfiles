;;; my-core-keybinds.el --- Keybinding scaffolding -*- lexical-binding: t; -*-

;;; Commentary:
;; which-key and general leader bindings that everything else relies on.

;;; Code:

(use-package which-key
  :straight t
  :config
  (setq which-key-idle-delay 0.1
        which-key-popup-type 'minibuffer)
  (which-key-mode))

(use-package general
  :straight t
  :after (which-key)
  :config
  (general-create-definer my-local-leader-def :prefix ",")
  (general-create-definer my-leader-def :prefix "SPC")

  (my-local-leader-def
    :states 'normal
    :keymaps 'org-mode-map
    "c" '(:ignore t :which-key "capture/clock")
    "cc" 'org-capture
    "cI" 'org-clock-in
    "cO" 'org-clock-out
    "s" 'org-schedule
    "." 'org-time-stamp-inactive
    "," 'org-ctrl-c-ctrl-c
    "r" 'org-refile
    "q" 'org-set-tags-command
    "ns" 'org-narrow-to-subtree
    "nw" 'widen
    "ta" 'org-roam-tag-add
    "tk" 'org-roam-tag-remove
    "tt" 'org-roam-buffer-toggle
    "it" 'my/org-insert-inactive-timestamp-now)

  (my-local-leader-def 'normal org-capture-mode-map
    "," 'org-capture-finalize)

  (general-def org-super-agenda-header-map
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line)

  (general-def org-agenda-mode-map
    "R" 'org-agenda-refile
    "r" 'org-agenda-refile)

  (general-def minibuffer-mode-map
    "C-j" 'vertico-next
    "C-k" 'vertico-previous
    "C-h" 'backward-kill-word)

  (general-def deft-mode-map
    "C-j" 'next-line
    "C-k" 'previous-line)

  (general-define-key
   :keymaps '(org-mode-map)
   :states '(normal)
   "t" 'org-todo
   "SPC h i" 'org-insert-heading-respect-content
   "SPC h s" 'org-insert-subheading
   "TAB" 'org-cycle
   ">" 'org-demote-subtree
   "<" 'org-promote-subtree
   "C-j" (lambda () (interactive) (delete-indentation t)))

  ;; Global
  (my-leader-def
    :keymaps 'override
    :states 'normal
    "" '(nil :which-key "my lieutenant general prefix")

    ;; Bookmarks
    "bs" 'bookmark-set
    "bj" 'bookmark-jump

    "acc" 'calc
    "acq" 'quick-calc

    ;; Org Roam
    "nrf" 'org-roam-node-find
    "nrdt" 'org-roam-dailies-goto-today

    ;; M-x
    "SPC" 'execute-extended-command

    ;; File commands
    "f"   '(:ignore t :which-key "files")
    "ff"  'find-file
    "fs"  'save-buffer
    "fj"  'my/dired-this-buffer

    ;; Window commands
    "w"   '(:ignore t :which-key "window/workspace")
    "w=" 'balance-windows
    "wm" 'delete-other-windows
    "wd" 'delete-window
    "ws" 'split-window-below
    "wv" 'split-window-right
    "wl" 'windmove-right
    "wh" 'windmove-left
    "wk" 'windmove-up
    "wj" 'windmove-down
    "wF" 'make-frame
    "wf" 'make-frame
    "wo" 'other-frame
    "fk" 'delete-frame

    ;; Avy commands
    "j" 'avy-goto-char

    ;; Buffer commands
    "b"   '(:ignore t :which-key "buffer")
    "TAB" 'switch-to-last-buffer
    "bb"  'consult-buffer
    "bd"  'kill-current-buffer
    "br"  'revert-buffer
    "bn"  'evil-buffer-new

    ;; Toggle commands
    "t"  '(:ignore t :which-key "toggle")
    "tf" 'toggle-frame-fullscreen
    "tt" 'my/theme-switcher

    ;; Help
    "h"  '(:ignore t :which-key "help")
    "hk" 'describe-key
    "hf" 'describe-function
    "hv" 'describe-variable
    "hm" 'describe-mode

    ;; Org
    "no" 'org-clock-goto

    ;; Project commands
    "g"  '(:ignore t :which-key "git")
    "gs" 'magit-status
    "/" 'consult-ripgrep

    "p"  '(:ignore t :which-key "project")
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project

    ;; Emacs
    "q"  '(:ignore t :which-key "quit")
    "fer" 'reload-config
    "fed" 'open-config-file
    "qq" 'save-buffers-kill-emacs
    "qr" 'restart-emacs

    ;; Compile
    "cm" 'compile--make)

  (general-define-key "C-;" 'comment-or-uncomment-region)
  (general-define-key "C-c n i" 'org-roam-node-insert))

(provide 'my-core-keybinds)

;;; my-core-keybinds.el ends here
