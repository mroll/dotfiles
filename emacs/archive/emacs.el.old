;; -*- lexical-binding: t -*-
;;; package --- matt's emacs config

;;; Commentary:
;;

;;; Code:


;; Encoding Config
;; ---------------
(prefer-coding-system 'utf-8-unix)
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8) ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8) ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8) ; configured by prefer-coding-system
(setq buffer-file-coding-system 'utf-8) ; utf-8-unix
(setq save-buffer-coding-system 'utf-8-unix) ; nil
(setq process-coding-system-alist
  (cons '("grep" utf-8 . utf-8) process-coding-system-alist))

(add-function :after after-focus-change-function
  (defun me/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))

;; Font Config
;; -----------
(set-face-attribute 'default nil :height 142)
(setq-default line-spacing .1)
;; (set-face-attribute 'default nil :font "Jetbrains Mono")
;; (set-frame-font "Jetbrains Mono" nil t)

;; Frame Config
;; ------------
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 150))

(scroll-bar-mode -1)
(menu-bar-mode   -1)
(tool-bar-mode   -1)

(add-to-list 'default-frame-alist'(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist'(ns-appearance . light))

(setq frame-title-format nil)

(when (equal system-type 'darwin)
  (setq ns-auto-hide-menu-bar t)
  (setq ns-use-proxy-icon nil)
  (setq initial-frame-alist
     (append
      '((ns-transparent-titlebar . t)
        (ns-appearance . light)
        (vertical-scroll-bars . nil)
        (internal-border-width . 0)))))


;; Misc Emacs Config
;; -----------------
(server-start)

(setq backup-directory-alist '(("." . "~/.emacs.d/.backups/")))

(setq vc-follow-symlinks t)

(setq org-image-actual-width nil)

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; Stop helm-org-rifle from complaining
(setq warning-suppress-types '((emacs)))

(setq ring-bell-function 'ignore)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq set-mark-command-repeat-pop t)
(setq track-eol t)
(setq line-move-visual nil)
(setq-default kill-whole-line t)
(setq-default indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq custom-safe-themes t)

;; Resource Config
;; ---------------
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setenv "NODE_OPTIONS" "--max-old-space-size=8192")


;; Startup Config
;; --------------
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)


;; Straight.el Config
;; ------------------
(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
  (url-retrieve-synchronously
   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
   'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'org)



;; Packages
;; ------------------------

(add-to-list 'load-path "/Users/mattroll/dotfiles/emacs/lisp/beancount-mode")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
(add-hook 'beancount-mode-hook #'outline-minor-mode)
(define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)


(defun insert-beancount-investment-contribution (cash-acct cash-amount currency ticker-acct ticker-shares ticker)
  (let* ((cash-debit (format "%s    %s %s" cash-acct cash-amount currency))
         (cash-credit (format "%s    -%s %s" cash-acct cash-amount currency))
         (ticker-price (format "%0.5f"(/ cash-amount ticker-shares)))
         (ticker-debit (format "%s    %s %s {%s USD}" ticker-acct ticker-shares ticker ticker-price)))
    (end-of-line)
    (newline-and-indent)
    (insert cash-debit)
    (newline-and-indent)
    (insert cash-credit)
    (newline-and-indent)
    (insert ticker-debit)))


(defun insert-beancount-investment-contribution--mattstripe401k (cash-amount ticker-shares)
  (interactive "nCash Amount: \nnTicker Shares: ")
  (let ((cash-account "Assets:US:Fidelity:MattStripe401k:Cash")
        (ticker-account "Assets:US:Fidelity:MattStripe401k:VTTSX")
        (currency "USD")
        (ticker "VTTSX"))
    (insert-beancount-investment-contribution cash-account cash-amount currency ticker-account ticker-shares ticker)))


(use-package add-node-modules-path
  :straight t
  :hook (rjsx-mode . add-node-modules-path))

(use-package alert
  :straight t
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(use-package all-the-icons
  :straight t)

(use-package ansi-color
  :straight t
  :config
  (defun display-ansi-colors ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package autorevert
  :straight t
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-use-notify nil)
  (setq auto-revert-verbose t))

(use-package avy
  :straight t
  :config
  (global-set-key (kbd "C-c j") 'avy-goto-char))


(use-package general
  :straight t
  :after (which-key)
  :config

  (general-create-definer my-local-leader-def :prefix ",")
  (general-create-definer my-leader-def :prefix "SPC")

  ;; Org Mode
  ;; ---
  (defun mpr/org-open-line ()
    (interactive)
    (evil-org-eol-call 'always-insert-item))

  (my-local-leader-def
    :states 'normal
    :keymaps 'org-mode-map
    "c" 'org-capture
    "s" 'org-schedule
    "I" 'org-clock-in
    "O" 'org-clock-out
    "." 'org-time-stamp-inactive
    "," 'org-ctrl-c-ctrl-c
    "r" 'org-refile
    "R" 'helm-org-rifle
    "q" 'org-set-tags-command

    "ns" 'org-narrow-to-subtree
    "nw" 'widen

    "ta" 'org-roam-tag-add
    "tk" 'org-roam-tag-remove
    "tt" 'org-roam-buffer-toggle)

  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   "o" 'mpr/org-open-line)

  (my-local-leader-def 'normal org-capture-mode-map
    "," 'org-capture-finalize)

  (general-def org-agenda-mode-map
    "R" 'org-agenda-refile
    "r" 'org-agenda-refile)

  ;; Company Mode
  ;; ---
  (general-define-key
   :keymaps '(company-active-map company-search-map)
    "C-j" 'company-select-next
    "C-k" 'company-select-previous)

  (general-def company-active-map
    "C-w" 'backward-kill-word
    "RET" 'company-complete-selection)

  (general-def treemacs-mode-map
    "SPC wm" 'windmove-right)

  (general-define-key
   :states 'insert
   :keymaps 'vterm-mode-map
     "C-e"     'vterm--self-insert
     "C-f"     'vterm--self-insert
     "C-a"     'vterm--self-insert
     "C-v"     'vterm--self-insert
     "C-b"     'vterm--self-insert
     "C-w"     'vterm--self-insert
     "C-u"     'vterm--self-insert
     "C-d"     'vterm--self-insert
     "C-n"     'vterm--self-insert
     "C-m"     'vterm--self-insert
     "C-p"     'vterm--self-insert
     "C-j"     'vterm--self-insert
     "C-k"     'vterm--self-insert
     "C-r"     'vterm--self-insert
     "C-t"     'vterm--self-insert
     "C-g"     'vterm--self-insert
     "C-c"     'vterm--self-insert
     "C-SPC"   'vterm--self-insert)

  (general-define-key
   :states 'normal
   :keymaps 'vterm-mode-map
     "C-d"        'vterm--self-insert
     ",c"         'multi-vterm
     ",n"         'multi-vterm-next
     ",p"         'multi-vterm-prev
     "i"          'evil-insert-resume
     "o"          'evil-insert-resume
     "<return>"   'evil-insert-resume)

  (general-define-key
   :states 'normal
   :keymaps 'beancount-mode-map
     "TAB"        'beancount-tab-dwim)

  ;; Global
  ;; ---
  (my-leader-def
    :keymaps 'override
    :states 'normal
    "" '(nil :which-key "my lieutenant general prefix")

    "acc" 'calc
    "acq" 'quick-calc

    "ft" 'treemacs
    "fd" 'treemacs-select-directory

    "tt"   'vterm-toggle
    "s-j"  'vterm-toggle

    ;; M-x
    "SPC" 'helm-M-x

    ;; Application commands
    "a"   '(:ignore t :which-key "application")
    "aoc" 'org-capture
    "aoa" 'org-agenda
    ;; "am"  'mu4e

    ;; File commands
    "f"   '(:ignore t :which-key "files")
    "ff"  'helm-find-files
    "fs"  'save-buffer
    "fer" 'reload-config
    "fed" 'open-config-file
    "fj"  'dired-this-buffer

    ;; Window commands
    "w"   '(:ignore t :which-key "window/workspace")
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

    ;; Workspace commands
    "w<" 'eyebrowse-prev-window-config
    "w>" 'eyebrowse-next-window-config
    "w," 'eyebrowse-rename-window-config
    "w0" 'eyebrowse-switch-to-window-config-0
    "w1" 'eyebrowse-switch-to-window-config-1
    "w2" 'eyebrowse-switch-to-window-config-2
    "w3" 'eyebrowse-switch-to-window-config-3
    "w4" 'eyebrowse-switch-to-window-config-4
    "w5" 'eyebrowse-switch-to-window-config-5
    "w6" 'eyebrowse-switch-to-window-config-6
    "w7" 'eyebrowse-switch-to-window-config-7
    "w8" 'eyebrowse-switch-to-window-config-8
    "w9" 'eyebrowse-switch-to-window-config-9

    ;; Buffer commands
    "b"   '(:ignore t :which-key "buffer")
    "TAB" 'switch-to-last-buffer
    "bb"  'helm-mini
    "bd"  'kill-this-buffer
    "ss"  'helm-swoop-without-pre-input
    "br"  'revert-buffer
    "bn"  'evil-buffer-new

    ;; Frame commands
    "tf" 'toggle-frame-fullscreen

    ;; Project commands
    "g"  '(:ignore t :which-key "git")
    "gs"  'magit-status
    "gr"  'mpr/refile-issue-from-project

    "p"  '(:ignore t :which-key "project")
    "pf"  'helm-projectile-find-file
    "pp"  'projectile-switch-project
    "/"   'helm-projectile-rg
    "pv"  'projectile-run-vterm

    ;; Jump commands
    "j"  '(:ignore t :which-key "jump")
    "jc" 'avy-goto-char

    ;; Service commands
    "s"  '(:ignore t :which-key "services")
    "sm" 'prodigy

    ;; Syntax commands
    "e"  '(:ignore t :which-key "errors")
    "el" 'flymake-show-buffer-diagnostics

    ;; Help
    "h"  '(:ignore t :which-key "help")
    "hk" 'describe-key
    "hf" 'describe-function
    "hv" 'describe-variable
    "hm" 'describe-mode

    ;; Emacs
    "q"  '(:ignore t :which-key "quit")
    "qq" 'save-buffers-kill-emacs
    "qr" 'restart-emacs

    ;; Notes
    "n"  '(:ignore t :which-key "notes")
    "nd" 'deft
    "nf" 'org-roam-node-find
    "ni" 'org-roam-node-insert)

  (general-def helm-grep-mode-map
    "TAB" 'helm-execute-persistent-action
    "C-i" 'helm-execute-persistent-action
    )

  (general-def helm-find-files-map
    "C-h" 'helm-find-files-up-one-level
    "C-l" 'helm-execute-persistent-action
    "TAB" 'helm-execute-persistent-action
    "C-j" 'helm-next-line
    "C-k" 'helm-previous-line)

  (general-define-key "C-;" 'comment-or-uncomment-region)
  (general-define-key "C-c n i" 'org-roam-node-insert-immediate)

  (defun mpr/agenda ()
    (interactive)
    (org-agenda nil "."))

  (general-define-key "s-j" 'vterm-toggle)
  (general-define-key "<f1>" 'mpr/agenda)
  (general-define-key "<f2>" 'org-roam-dailies-goto-today))

(use-package company
  :straight t
  :init
  (global-company-mode)
  (setq company-tooltip-limit 20)
  (setq company-show-numbers t)
  (setq company-idle-delay 0.1)
  (setq company-echo-delay 0)

  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-idle-delay 0.0)
  (setq company-tooltip-minimum-width 60)
  (setq company-tooltip-margin 2))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icon-right-margin 1))

(use-package company-flow
  :straight t
  :config
  (add-to-list 'company-backends 'company-flow))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-banner-logo-title "Happy Hacking :)"))

(use-package deft
  :straight t
  :after (evil)
  :config
  (setq deft-default-extension "org")
  (setq deft-directory "~/org/notes")
  (setq deft-recursive t)
  (setq deft-org-mode-title-prefix t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0)

  (setq deft-file-naming-rules
      '((noslash . "-")
        (nospace . "-")
        (case-fn . downcase)))

  (add-hook 'deft-mode-hook #'evil-insert-state)

  (define-key deft-mode-map (kbd "C-k") 'quit-window)
  (evil-define-key 'insert deft-mode-map (kbd "C-k") 'quit-window)

  (defun mpr/deft-notes-popup ()
    (let ((monitor-start-x (nth 1 (assq 'geometry (frame-monitor-attributes))))
          (monitor-width (nth 3 (assq 'geometry (frame-monitor-attributes)))))
      (make-frame-on-current-monitor '((name . "Deft Notes")
                                       (width . 80)
                                       (height . 50)))
      (set-frame-position (selected-frame)
                          (-
                           (+ monitor-start-x monitor-width)
                           700)
                          50)
      (deft)
      (evil-insert-state))))

(use-package doom-modeline
  :straight t
  :init
  (line-number-mode 1)
  (column-number-mode 1)

  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)

  (display-time-mode 1)
  (display-battery-mode 1)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-time t)
  (setq display-time-default-load-average nil)
  (setq display-time-format "%I:%M%p    ")

  (setq global-mode-string nil)

  (doom-modeline-mode 1))

(use-package doom-themes
  :straight t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (doom-themes-org-config)

  (load-theme 'doom-one-light)
  )

;; (use-package eglot
;;   :straight t
;;   :config
;;   (add-hook 'rjsx-mode-hook 'eglot-ensure)
;;   (add-hook 'web-mode-hook 'eglot-ensure)
;;   (add-hook 'js2-mode-hook 'eglot-ensure)
;;   )

(use-package elpy
  :straight t
  :init
  (elpy-enable)

  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i"))

(use-package emacsql-sqlite3
  :straight t)

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :straight t
  :custom (evil-collection-mode-list
     '(magit calc calendar dired flycheck helm ibuffer ivy))
  :config
  (evil-collection-init))

(use-package evil-escape
  :straight t
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "kj"))

;; (use-package evil-leader
;;   :straight t
;;   :config
;;   (global-evil-leader-mode)
;;   (evil-leader/set-leader "<SPC>")
;;
;;   (define-key evil-normal-state-map (kbd "C-u") 'scroll-down-half-page)
;;   (define-key evil-normal-state-map (kbd "C-d") 'scroll-up-half-page))

(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)

  (setf evil-org-key-theme '(navigation insert textobjects additional))

  (evil-org-agenda-set-keys))

(use-package eyebrowse
  :straight t
  :config
  (eyebrowse-mode t))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package flow-minor-mode
  :straight t
  :config
  (add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
  (add-hook 'rjsx-mode-hook 'flow-minor-enable-automatically))

(use-package flymake
  :ensure t
  :config
  (add-hook 'rjsx-mode-hook 'flymake-mode))

(use-package fzf
  :straight t
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "rg --no-heading -nH"
        fzf/position-bottom t
        fzf/window-height 25))

(use-package git-auto-commit-mode
  :straight t
  :config
  (setq-default gac-automatically-add-new-files-p t)
  (setq-default gac-automatically-push-p t)

  ;; minimum 1 hour between commits
  (setq-default gac-debounce-interval 3600))


(use-package git-gutter
  :straight t
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  :custom-face
  (git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
  (git-gutter:added    ((t (:foreground "#50fa7b" :background "#50fa7b"))))
  (git-gutter:deleted  ((t (:foreground "#ff79c6" :background "#ff79c6"))))
  :config
  (global-git-gutter-mode +1))

(use-package helm
  :straight t
  :config
  (helm-mode 1)

  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "M-d") 'mpr/helm-rg--set-dir)


  (setq helm-input-idle-delay 0)
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-bookmarks
                                    helm-source-recentf
                                    helm-source-buffer-not-found))

  (setq helm-split-window-in-side-p t))

(use-package helm-org-rifle
  :straight t
  :config
  (setq helm-org-rifle-show-path t))

(use-package helm-projectile
  :straight t
  :config
  (helm-projectile-on)

  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-directories '("node_modules"
                                               "target")))

(use-package helm-rg
  :straight t
  :config

  (setq helm-rg-thing-at-point nil))

(use-package helm-swoop
  :straight t)

(use-package hide-mode-line
  :straight t)

(use-package highlight-indent-guides
  :straight t
  :diminish
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character))

(use-package hl-line
  :straight t
  :hook
  (after-init . global-hl-line-mode))

(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-basic-offset 2)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t)

  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  ;; Better imenu
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package kaolin-themes
  :straight t
  :config
  ;; (load-theme 'kaolin-mono-light t)
  (kaolin-treemacs-theme))

(use-package lsp-mode
  :straight t
  :commands lsp
  :config
  (add-hook 'ruby-mode-hook #'lsp)
  (add-hook 'enh-ruby-mode-hook #'lsp)
  (add-hook 'rjsx-mode-hook #'lsp)
  (setq lsp-prefer-flymake :none)
  (setq lsp-log-io t)
  (setq lsp-enable-snippet nil)
  (setq lsp-restart 'ignore))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-peek-list-width 125)
  (setq lsp-ui-imenu-window-width 70)
  (setq lsp-ui-imenu-window-fix-width t)

  (evil-define-key nil lsp-ui-peek-mode-map (kbd "C-j") 'lsp-ui-peek--select-next)
  (evil-define-key nil lsp-ui-peek-mode-map (kbd "C-k") 'lsp-ui-peek--select-prev)
  (evil-define-key nil lsp-ui-peek-mode-map (kbd "C-h") 'lsp-ui-peek--select-prev-file)
  (evil-define-key nil lsp-ui-peek-mode-map (kbd "C-l") 'lsp-ui-peek--select-next-file))

(use-package company-lsp
  :straight t
  :commands company-lsp)

(use-package lua-mode
  :straight t)

(use-package magit
  :straight t
  :config

  (setq magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)

  ;; Uncomment for magit profiling
  ;; https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
  ;; (setq magit-refresh-verbose t)

  (setq vc-handled-backends nil)

  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-sequencer-sequence)
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)


  (evil-define-key 'normal with-editor-mode-map (kbd ",,") 'with-editor-finish)
  (evil-define-key 'motion with-editor-mode-map (kbd ",,") 'with-editor-finish))

(use-package magit-delta
  :straight t
  :hook (magit-mode . magit-delta-mode))

(use-package multi-vterm
  :straight t
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state)))
  ;; (define-key vterm-mode-map [return]                      #'vterm-send-return)

  ;; (setq vterm-keymap-exceptions nil)
  )

(use-package org-roam
  :straight t
  :after (org)
  :config

  (setq org-roam-directory (expand-file-name "~/org/org-roam"))
  (setq org-roam-database-connector 'sqlite3)
  (setq org-roam-dailies-directory "daily/")

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))

  (org-roam-db-autosync-mode)

  (setq org-roam-capture-templates
        '(("d" "default" plain
           "\n\n\n%?\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t
           )))

  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args))))

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org
  :straight t
  :config
  (require 'org-agenda)
  (require 'org-capture)
  (require 'org-habit)
  (require 'org-tempo)

  (require 'ob-ruby)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ruby . t)))

  (defun mpr/org-confirm-babel-evaluate (lang body)
    (not (or (string= lang "ruby"))))
  (setq org-confirm-babel-evaluate 'mpr/org-confirm-babel-evaluate)

  (setq mpr/org-dir "~/org/")
  (setq mpr/org-agenda-directory mpr/org-dir)

  (setq mpr/org-agenda-files `(,(concat mpr/org-agenda-directory "life.org")
                               ,(concat mpr/org-agenda-directory "inbox.org")
                               ,(concat mpr/org-agenda-directory "org-roam/")
                               ))
  (setq org-agenda-files mpr/org-agenda-files)
  (setq org-hide-emphasis-markers t)
  (setq org-use-speed-commands t)
  (setq org-directory mpr/org-dir)
  (setq org-default-notes-file (concat mpr/org-dir "inbox.org"))
  (setq org-agenda-window-setup 'only-window)

  (add-to-list 'org-capture-templates
               `("i" "inbox" entry (file ,(concat mpr/org-agenda-directory "inbox.org"))
                 "* TODO %?\n  %U"))
  (add-to-list 'org-capture-templates
               `("t" "tab" entry (file ,(concat mpr/org-agenda-directory "inbox.org"))
                 "* TODO %i\n  %U\n\n  %c"))

  (setq org-tag-alist (quote (("productivity" . ?p)
                              ("research" . ?s)
                              ("reading" . ?r)
                              ("writing" . ?w)
                              ("hammerspoon" . ?h)
                              ("staff" . ?t)
                              ("emacs" . ?e))))

  (setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

  (setq org-clock-history-length 23)
  (setq org-clock-in-resume t)

  (setq org-clock-out-remove-zero-time-clocks t)

  (setq org-clock-out-when-done t)
  (setq org-clock-persist t)
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  (setq org-clock-report-include-clocking-task t)

  (setq org-clock-idle-time 5)

  (setq org-enforce-todo-dependencies t)

  (setq org-hide-leading-stars t)

  (org-indent-mode -1)
  (setq org-startup-indented nil)

  (setq org-cycle-separator-lines 0)

  (setq org-blank-before-new-entry (quote ((heading)
             (plain-list-item . auto))))

  (setq org-insert-heading-respect-content t)

  (setq org-reverse-note-order nil)

  (setq org-show-following-heading t)
  (setq org-show-hierarchy-above t)
  (setq org-show-siblings (quote ((default))))

  (setq org-log-done (quote time))
  (setq org-log-into-drawer t)
  (setq org-log-state-notes-insert-after-drawers nil)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c@)")
                (sequence "WAITING(w)" "SOMEDAY(s)" "|"))))

  (add-to-list 'org-modules 'org-habit t)
  (setq org-habit-show-all-today t)
  (setq org-habit-graph-column 60)

  (setq org-todo-keyword-faces
  (quote (("TODO" :foreground "orange" :weight bold)
    ("NEXT" :foreground "green3" :weight bold)
    ("DONE" :foreground "gray38" :weight bold)
    ("WAITING" :foreground "magenta" :weight bold)
    ("CANCELLED" :foreground "gray38" :weight bold))))

  (defun bh/find-project-task ()
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun mpr/is-project-p ()
    (and
     (equal (org-get-todo-state) "TODO")
     (equal (org-get-category) "project")))

  (defvar bh/hide-scheduled-and-waiting-next-tasks t)

  (defun bh/is-project-p ()
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun bh/is-project-subtree-p ()
    "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
      (save-excursion
        (bh/find-project-task)
        (if (equal (point) task)
            nil
          t))))

  (defun bh/is-task-p ()
    "Any task with a todo keyword and no subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))

  (defun bh/is-subproject-p ()
    "Any task which is a subtask of another project"
    (let ((is-subproject)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq is-subproject t))))
      (and is-a-task is-subproject)))

  (defun bh/skip-stuck-projects ()
    "Skip trees that are not stuck projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (mpr/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  nil
                next-headline))
          nil))))

  (defun bh/skip-non-stuck-projects ()
    "Skip trees that are not stuck projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (mpr/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  next-headline
                nil))
          next-headline))))

  (defun bh/skip-project-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((mpr/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         ((bh/is-project-subtree-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-non-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((bh/is-task-p)
          nil)
         (t
          next-headline)))))

  (defun bh/skip-non-projects ()
    "Skip trees that are not projects"
    (if (save-excursion (bh/skip-non-stuck-projects))
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((mpr/is-project-p)
              nil)
             ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
              nil)
             (t
              subtree-end))))
      (save-excursion (org-end-of-subtree t))))

  (defun mpr/skip-non-chore-tasks ()
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (and (equal (org-get-category) "todo")
               (not (equal (org-get-todo-state) "WAITING")))
          nil
        subtree-end)))

  (defun mpr/skip-non-project-tasks ()
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (and (equal (org-get-category) "project")
               (not (equal (org-get-todo-state) "WAITING")))
          nil
        subtree-end)))

  (setq org-agenda-tags-todo-honor-ignore-options t)

  (setq mpr/org-agenda-todo-view
        `("." "Agenda"
          ((agenda ""
                   ((org-agenda-span 'day)
                    (org-deadline-warning-days 365)))
           (todo "TODO"
                 ((org-agenda-overriding-header "Refile")
                  (org-agenda-files `(,(concat mpr/org-agenda-directory "inbox.org")))))
           (tags-todo "-TODO=\"TODO\"-CANCELLED-WAITING"
                      ((org-agenda-overriding-header "Project Tasks")
                       (org-agenda-skip-function 'mpr/skip-non-project-tasks)
                       (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                       (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                       (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                       (org-agenda-tag-filter '("-productivity"))
                       (org-agenda-sorting-strategy
                        '(category-keep))))
           (tags-todo "-productivity-research-staff-REFILE-CANCELLED-WAITING-HOLD/!"
                      ((org-agenda-overriding-header "Todo")
                       (org-agenda-skip-function 'mpr/skip-non-chore-tasks)
                       (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                       (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                       (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                       (org-agenda-files `(,(concat mpr/org-agenda-directory "life.org")))
                       (org-agenda-sorting-strategy
                        '(category-keep))))
           (tags-todo "productivity"
                 ((org-agenda-overriding-header "Productivity")))
           (tags-todo "research"
                 ((org-agenda-overriding-header "Research")))
           (tags-todo "-WAITING-CANCELLED/!"
                      ((org-agenda-overriding-header "Projects")
                       (org-agenda-skip-function 'bh/skip-non-projects)
                       (org-agenda-files `(,(concat mpr/org-agenda-directory "life.org")))
                       (org-agenda-sorting-strategy
                        '(category-keep))))
           (todo "WAITING"
                 ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                        (if bh/hide-scheduled-and-waiting-next-tasks
                                                            ""
                                                          " (including WAITING and SCHEDULED tasks)")))
                  (org-agenda-skip-function 'bh/skip-non-tasks)
                  (org-tags-match-list-sublevels nil)
                  (org-agenda-files `(,(concat mpr/org-agenda-directory "life.org")))
                  (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
           nil)))

  (setq mpr/org-agenda-month-view
        `("m" "Month"
          ((agenda ""
                   ((org-agenda-span 'month)
                    (org-deadline-warning-days 365))))))

  (setq org-agenda-custom-commands `(,mpr/org-agenda-todo-view
                                     ,mpr/org-agenda-month-view))

  (defun gtd-save-org-buffers ()
    "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
    (interactive)
    (message "Saving org-agenda-files buffers...")
    (save-some-buffers t (lambda ()
                           (when (string-match "\.org$" (buffer-file-name))
                             t)))
    (message "Saving org-agenda-files buffers... done"))

  (advice-add 'org-refile :after
              (lambda (&rest _)
                (gtd-save-org-buffers)))

  (defun mpr/org-capture-tab (title url)
    (with-temp-buffer
      (insert url)
      (evil-delete-whole-line (point-min) (point-max)))
    (org-capture-string title "t")
    (org-capture-finalize))

  (setq org-adapt-indentation t)

  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(("~/org/stripe.org" :maxlevel . 10)
                             ("~/org/someday.org" :maxlevel . 10)
                             ("~/org/readlater.org" :maxlevel . 10)))

  (org-clock-persistence-insinuate)

  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  (defvar bh/insert-inactive-timestamp t)

  (defun bh/toggle-insert-inactive-timestamp ()
    (interactive)
    (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
    (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

  (defun bh/insert-inactive-timestamp ()
    (interactive)
    (org-insert-time-stamp nil t t nil nil nil))

  (defun bh/insert-heading-inactive-timestamp ()
    (save-excursion
      (when bh/insert-inactive-timestamp
        (org-return)
        (org-cycle)
        (bh/insert-inactive-timestamp))))

  (add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

  (evil-define-key 'normal org-mode-map "t" 'org-todo)

  (evil-define-key 'normal org-mode-map (kbd "SPC h i") 'org-insert-heading)
  (evil-define-key 'normal org-mode-map (kbd "SPC h s") 'org-insert-subheading)
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)

  (evil-define-key 'normal org-mode-map (kbd ">") 'org-demote-subtree)
  (evil-define-key 'normal org-mode-map (kbd "<") 'org-promote-subtree))

(use-package org-autolist
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode))))

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))

(use-package parrot
  :straight t
  :config
  (parrot-mode))

;; (use-package persp-projectile
;;   :straight t)

;; (use-package perspective
;;   :straight t
;;   :config
;;   (setq persp-mode-prefix-key (kbd "C-x x"))
;;   (persp-mode))

(use-package popwin
  :straight t
  :config
  (popwin-mode 1))

(use-package prettier-js
  :straight t
  :init

  (defun enable-minor-mode (my-pair)
    (if (buffer-file-name)
        (if (string-match (car my-pair) buffer-file-name)
            (funcall (cdr my-pair)))))

  (add-hook 'web-mode-hook #'(lambda () (enable-minor-mode '("\\.jsx?\\'" . prettier-js-mode))))
  (add-hook 'typescript-mode-hook #'(lambda () (enable-minor-mode '("\\.tsx?\\'" . prettier-js-mode))))
  (add-hook 'rjsx-mode-hook #'(lambda () (enable-minor-mode '("\\.tsx?\\'" . prettier-js-mode)))))

(use-package prodigy
  :straight t
  :config

  (evil-define-key 'normal prodigy-mode-map (kbd "q") 'quit-window)
  (evil-define-key 'normal prodigy-mode-map (kbd "s") 'prodigy-start)
  (evil-define-key 'normal prodigy-mode-map (kbd "S") 'prodigy-stop)
  (evil-define-key 'normal prodigy-mode-map (kbd "gr") 'prodigy-restart)
  (evil-define-key 'normal prodigy-mode-map (kbd "d") 'prodigy-display-process)

  (evil-define-key 'normal prodigy-view-mode-map (kbd "q") 'quit-window)
  (prodigy-define-service
    :name "canopy-charts-lib"
    :command "npx"
    :args '("webpack" "--watch")
    :cwd "~/src/canopy-charts-lib/"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "api.canopycharts.com"
    :command "yarn"
    :args '("run" "nodemon" "index.ts")
    :port 3000
    :cwd "~/src/app.canopycharts.com/server/"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "app.canopycharts.com"
    :command "yarn"
    :args '("start")
    :port 3001
    :cwd "~/src/app.canopycharts.com"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "canopycharts.com"
    :command "yarn"
    :args '("start")
    :port 3002
    :cwd "~/src/canopycharts.com"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "make-emacs-awesome"
    :command "yarn"
    :args '("start")
    :port 3003
    :cwd "~/src/make-emacs-awesome"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)

  )

(use-package projectile
  :init
  (setq projectile-indexing-method 'alien)
  (setq projectile-use-git-grep t)
  (setq projectile-tags-command "/usr/local/bin/ctags --exclude=node_modules --exclude=admin --exclude=.git --exclude=frontend --exclude=home --exclude=**/*.js -Re -f \"%s\" %s")

  :config
  (projectile-global-mode))

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
  (defun ladicle/recentf-save-list-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if (fboundp 'shut-up)
          (shut-up (recentf-save-list))
        (recentf-save-list)))
    (message ""))

  (defun ladicle/recentf-cleanup-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if shutup-p
          (shut-up (recentf-cleanup))
        (recentf-cleanup)))
    (message ""))
  :hook
  (focus-out-hook . (ladicle/recentf-save-list-silence ladicle/recentf-cleanup-silence)))

(use-package restart-emacs
  :straight t)

(use-package rjsx-mode
  :straight t
  :mode ("\\.jsx\\'" "\\.tsx\\'"))

(use-package ruby-mode
  :config
  (defun my-ruby-mode-hook ()
    (set-fill-column 80)
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)
    (setq ruby-insert-encoding-magic-comment nil))
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))

(use-package s
  :straight t)

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package slack
  :straight t
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :config

  (slack-register-team
   :name "czitechnologies"
   :default t
   :token "xoxc-1824813693126-1844217614177-4654612160754-2dd86b9fb4868c49d747e67f41c9f6db7c10962c9670a870a86274ff686428fd"
   :cookie "xoxd-fQhIPvOTwWwZofGX6lFJydGg6S8Hea0wVIolnqxvISJEx4XUUF7C%2BJNITfOIZ5rHPx83zqzQKTZHsABNQkYcPX0%2BMfa8Kb6yi7FxWwTn44EqoX6xjdIPNP%2FihZDOiFoB2LUPB%2BcPvOjEygxjshy0Nt2Au8PfHvIWYa4fM0OhQVuD0cDVJF2kWtWHmA%3D%3D"
   :subscribed-channels '(general)
   :full-and-display-names t)

  (evil-define-key 'normal slack-info-mode-map
    ",u" 'slack-room-update-messages)
  (evil-define-key 'normal slack-mode-map
    ",c" 'slack-buffer-kill
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete
    ",u" 'slack-room-update-messages
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message)
  (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel))

(use-package smartparens
  :straight t
  :init
  (require 'smartparens-config)
  :hook
  (after-init . smartparens-global-mode)
  :config
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

(use-package terraform-mode
  :straight t)

(use-package treemacs
  :straight t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               100
          treemacs-width                           50
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :straight t
  :after (treemacs evil))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :straight t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :straight t
  :after (treemacs magit))

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :straight t
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :config (treemacs-set-scope-type 'Perspectives))

(use-package typescript-mode
  :straight t)

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package vterm
  :straight t)

(use-package vterm-toggle
  :straight t)

(use-package web-mode
  :straight t
  :init
  (defun web-mode-customization ()
    "Customization for web-mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-colorization t)
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local))
  (add-hook 'web-mode-hook 'web-mode-customization)

  :mode ("\\.html?\\'" "\\.erb\\'" "\\.hbs\\'"
         "\\.json\\'" "\\.s?css\\'" "\\.less\\'" "\\.sass\\'"))

(use-package which-key
  :straight t
  :config
  (setq which-key-idle-delay 0.1)
  (setq which-key-popup-type 'minibuffer)

  (which-key-mode))

(use-package whitespace
  :straight t
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package yaml-mode
  :straight t)

(use-package zoom
  :straight t
  :config
  (zoom-mode 1))


;; Functions
;; ------------------------

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun open-config-file ()
  (interactive)
  (find-file "~/dotfiles/emacs/.emacs.el"))

(defun reload-config ()
  (interactive)
  (load-file "~/.emacs.el"))

(defun dired-this-buffer ()
  (interactive)
  (find-file default-directory))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
  (filename (buffer-file-name)))
    (if (not filename)
  (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
    (message "A buffer named '%s' already exists!" new-name)
  (progn
    (rename-file filename new-name 1)
    (rename-buffer new-name)
    (set-visited-file-name new-name)
    (set-buffer-modified-p nil))))))

(defun wc (&optional start end)
  "Prints number of lines, words and characters in region or whole buffer."
  (interactive)
  (let ((n 0)
        (start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
    (message "%3d %3d %3d" (count-lines start end) n (- end start))))

(defun always-insert-item ()
  (if (not (org-in-item-p))
      (insert "\n")
    (org-insert-item)))

(defun evil-org-eol-call (fun)
  (end-of-line)
  (funcall fun)
  (evil-append nil))

(use-package visual-fill-column
  :straight t)

(use-package writeroom-mode
  :straight t)

(defun blogging-mode ()
  (interactive)
  (variable-pitch-mode)
  (setq buffer-face-mode-face
        '(:family "ETBembo" :height 140))
  (visual-line-mode)
  (visual-fill-column-mode)
  (setq visual-fill-column-width 100)
  (org-indent-mode)
  (auto-fill-mode -1))

;; (let* ((variable-tuple
;;         (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;               ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;   (custom-theme-set-faces
;;    'user
;;    `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;; Hooks
;; ------------------------

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Emacs-managed Customizations
;; (I don't love this but it's hard to completely get rid of)
;; ---
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:foreground "#50fa7b" :background "#50fa7b"))))
 '(git-gutter:deleted ((t (:foreground "#ff79c6" :background "#ff79c6"))))
 '(git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#383a42" :font "ETBembo" :height 2.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#383a42" :font "ETBembo" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#383a42" :font "ETBembo" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#383a42" :font "ETBembo" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#383a42" :font "ETBembo" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#383a42" :font "ETBembo"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#383a42" :font "ETBembo"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#383a42" :font "ETBembo"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#383a42" :font "ETBembo"))))
 '(variable-pitch ((t (:family "ETBembo")))))
