(scroll-bar-mode -1)
(menu-bar-mode   -1)
(tool-bar-mode   -1)

(add-to-list 'default-frame-alist'(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist'(ns-appearance . light))

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
        (internal-border-width . 0)))))

(setq vc-follow-symlinks t)

(setq ring-bell-function 'ignore)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq set-mark-command-repeat-pop t)
(setq track-eol t)
(setq line-move-visual nil)
(setq warning-minimum-level :error)
(setq-default kill-whole-line t)
(setq-default indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq custom-safe-themes t)
(setq electric-pair-mode 1)

(set-face-attribute 'default nil
                    :family "Cascadia Code"
                    :height 140)


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

(use-package evil
  :straight t
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :straight t
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "kj"))

(use-package web-mode
  :straight t
  :mode ("\\.astro\\'" . web-mode)
  :config
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)

  (evil-org-set-key-theme '(textobjects insert additional shift todo heading))

  (evil-org-agenda-set-keys))

(use-package hl-line
  :straight t
  :hook
  (after-init . global-hl-line-mode))

(use-package restart-emacs
  :straight t)

(use-package whitespace
  :straight t
  :hook (before-save . whitespace-cleanup))

(use-package consult
  :straight t
  :config
  (setq consult-project-root-function #'projectile-project-root))

(use-package vertico
  :straight t
  ;; :bind (:map minibuffer-local-map
  ;;          ("C-j" . vertico-next)
  ;;          ("C-k" . vertico-previous))
  :custom
  (vertico-cycle t)
  (vertico-count 20)
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  :init
  (vertico-mode))

(use-package vertico-posframe
  :straight t
  :config
  (vertico-posframe-mode 1))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :straight t
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package which-key
  :straight t
  :config
  (setq which-key-idle-delay 0.1)
  (setq which-key-popup-type 'minibuffer)

  (which-key-mode))

(use-package general
  :straight t
  :after (consult which-key)
  :config

  (general-create-definer my-local-leader-def :prefix ",")
  (general-create-definer my-leader-def :prefix "SPC")

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
   "<" 'org-promote-subtree)

  ;; Global
  ;; ---
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

    ;; Notes
    ;; "nn" 'denote
    ;; "nr" 'denote-rename-file
    ;; "nl" 'denote-link
    ;; "nb" 'denote-backlinks
    ;; "nd" 'denote-dired
    ;; "ng" 'denote-grep
    ;; "nf" 'consult-notes

    ;; Avy commands
    "j" 'avy-goto-char

    ;; Buffer commands
    "b"   '(:ignore t :which-key "buffer")
    "TAB" 'switch-to-last-buffer
    "bb"  'consult-buffer
    "bd"  'kill-current-buffer
    "br"  'revert-buffer
    "bn"  'evil-buffer-new

    ;; Frame commands
    "tf" 'toggle-frame-fullscreen

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
    "gs"  'magit-status
    "/" 'consult-ripgrep

    "p"  '(:ignore t :which-key "project")
    "pf"  'projectile-find-file
    "pp"  'projectile-switch-project

    ;; Emacs
    "q"  '(:ignore t :which-key "quit")
    "fer" 'reload-config
    "fed" 'open-config-file
    "qq" 'save-buffers-kill-emacs
    "qr" 'restart-emacs

    ;; Compile
    "cm" 'compile--make
    )

  (general-define-key "C-;" 'comment-or-uncomment-region)
  (general-define-key "C-c n i" 'org-roam-node-insert)

  )

(use-package magit
  :straight t)

(use-package org-ql
  :straight t)

(use-package helm-org-ql
  :straight t)

(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic)))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :straight t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))


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

(use-package org-super-agenda
  :straight t
  :config
  (org-super-agenda-mode))

(use-package org-fancy-priorities
  :straight t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config

  (setq org-priority-highest 0
        org-priority-default 2
        org-priority-lowest 4)
  (setq org-fancy-priorities-list '(
                                    (?0 . "P0")
                                    (?1 . "P1")
                                    (?2 . "P2")
                                    (?3 . "P3")
                                    (?4 . "P4"))

        org-priority-faces '((?0 :foreground "DarkRed" :background "LightPink")
                             (?1 :foreground "DarkOrange4" :background "LightGoldenrod")
                             (?2 :foreground "gray20" :background "gray")
                             (?3 :foreground "gray20" :background "gray")
                             (?4 :foreground "gray20" :background "gray"))))

(use-package org
  :straight t
  :config
  (require 'org-tempo)

  (setq mroll/org-directory "~/org/")
  (setq mroll/org-agenda-directory mroll/org-directory)
  ;; (setq org-agenda-files '("~/org/stripe.org"))

  (setq org-hide-emphasis-markers t)
  (setq org-use-speed-commands t)
  (setq org-directory mroll/org-directory)
  (setq org-default-notes-file (concat mroll/org-directory "inbox.org"))
  (setq org-agenda-window-setup 'only-window)

  ;; Clock
  (setq org-clock-in-resume t)
  (setq org-clock-out-when-done t)
  (setq org-clock-persist t)
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  (setq org-clock-report-include-clocking-task t)
  (setq org-clock-idle-time 5)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-idle-time 5)

  (setq org-enforce-todo-dependencies t)

  (setq org-hide-leading-stars t)

  (org-indent-mode -1)
  (setq org-startup-indented nil)

  (setq org-cycle-separator-lines 0)

  (setq org-insert-heading-respect-content t)
  (setq org-log-into-drawer t)
  (setq org-log-state-notes-insert-after-drawers nil)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "IN PROGRESS(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (setq org-todo-keyword-faces
        '(("IN PROGRESS" . "green3")))
  (setq org-refile-targets (quote ((nil :maxlevel . 4)
                                   (org-agenda-files :maxlevel . 4))))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  ;; (setq mroll/org-agenda-todo-view
  ;;       `(" " "Agenda"
  ;;         ((agenda ""
  ;;                  ((org-agenda-span 'day)
  ;;                   (org-deadline-warning-days 365)))
  ;;          (todo "TODO"
  ;;                ((org-agenda-overriding-header "To Refile")
  ;;                 (org-agenda-files '(,(concat mroll/org-agenda-directory "inbox.org")))))
  ;;          (todo "IN PROGRESS"
  ;;                ((org-agenda-overriding-header "In Progress")
  ;;                 (org-agenda-files '(,(concat mroll/org-agenda-directory "stripe.org")))
  ;;                 ))
  ;;          (todo "" ((org-agenda-overriding-header "Project Tasks")
  ;;                    (org-super-agenda-groups
  ;;                     '((:auto-group t)
  ;;                       (:discard (:anything t))))))
  ;;          (todo "TODO"
  ;;                ((org-agenda-overriding-header "One-off Tasks")
  ;;                 (org-agenda-files '(,(concat mroll/org-agenda-directory "next.org")))
  ;;                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
  ;;          nil)))

  ;; (add-to-list 'org-agenda-custom-commands `,mroll/org-agenda-todo-view)
  )

(use-package projectile
  :straight t
  :init
  (setq projectile-indexing-method 'alien)
  (setq projectile-use-git-grep t)
  ;; (setq projectile-tags-command "/usr/local/bin/ctags --exclude=node_modules --exclude=admin --exclude=.git --exclude=frontend --exclude=home --exclude=**/*.js -Re -f \"%s\" %s")

  :config
  (projectile-global-mode))



;; Make sure straight and use-package are set up first
;; (I'll assume you already have that part)

;; Tuareg mode for OCaml editing
(use-package tuareg-mode
  :straight (:host github :repo "ocaml/tuareg")
  :mode (("\\.ml\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode)))

;; Merlin for smart completion, type info, goto
(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))
(use-package merlin
  :commands merlin-mode
  :hook ((tuareg-mode . merlin-mode)
         (caml-mode . merlin-mode))
  :custom
  (merlin-command 'opam)) ;; tell merlin to use opam env

;; Dune mode (optional, for dune files)
(use-package dune
  :straight t
  :mode ("\\`dune\\'" . dune-mode))

;; Optional: UTop for REPL
(use-package utop
  :straight t
  :hook (tuareg-mode . utop-minor-mode)
  :custom
  (utop-command "opam exec -- utop -emacs"))

(defun my-ocamlformat-buffer ()
  "Format the current OCaml buffer using ocamlformat."
  (interactive)
  (when (and buffer-file-name
             (or (eq major-mode 'tuareg-mode)
                 (eq major-mode 'caml-mode)))
    (let ((exit-code (call-process "ocamlformat" nil "*ocamlformat*" nil
                                   "--inplace"
                                   "--enable-outside-detected-project"
                                   "--name" (buffer-name)
                                   buffer-file-name)))
      (if (zerop exit-code)
          (progn
            (revert-buffer :ignore-auto :noconfirm)
            (message "Formatted successfully."))
        (progn
          (message "Ocamlformat failed. Check *ocamlformat* buffer for details."))))))

(add-hook 'tuareg-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'my-ocamlformat-buffer nil t)))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :config
  ;; Flycheck trigger tuning: delay checks until idle
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-idle-change-delay 0.5))

(use-package flycheck-ocaml
  :straight t
  :after (merlin flycheck)
  :config
  (flycheck-ocaml-setup))

;; Company-mode setup
(use-package company
  :straight t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)

  (setq company-backends '((company-capf)))

  )

(use-package lsp-mode
  :straight t
  :hook ((c-mode c++-mode) . lsp)
  :commands lsp)

(defun my-lsp-format-on-save ()
  "Add LSP formatting to before-save-hook."
  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

(add-hook 'lsp-mode-hook #'my-lsp-format-on-save)

;; Company + Merlin integration
(with-eval-after-load 'merlin
  (setq merlin-completion-with-doc t)
  (add-hook 'merlin-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'merlin-company-backend))))


;; (use-package denote
;;   :straight t
;;   :hook (dired-mode . denote-dired-mode)
;;   :config
;;   (setq denote-directory (expand-file-name "~/Documents/notes/"))

;;   ;; Automatically rename Denote buffers when opening them so that
;;   ;; instead of their long file name they have, for example, a literal
;;   ;; "[D]" followed by the file's title.  Read the doc string of
;;   ;; `denote-rename-buffer-format' for how to modify this.
;;   (denote-rename-buffer-mode 1))

(use-package deft
  :straight t
  :config
  (setq deft-directory "~/Documents/notes/")
  (setq deft-recursive t)
  (setq deft-auto-save-interval nil))

;; (use-package consult-notes
;;   :straight t
;;   :after denote
;;   :config
;;   (setq consult-notes-sources
;;         `(("Notes" ?n ,denote-directory)))
;;   (consult-notes-denote-mode))

(use-package cc-mode
  :ensure nil  ;; cc-mode is built into Emacs
  :hook
  (c-mode . my-c-mode-setup)
  :config
  (defun my-c-mode-setup ()
    (c-set-style "linux")
    (setq c-basic-offset 4
          indent-tabs-mode nil
          tab-width 4)
    (message "c-mode customizations loaded")))

(use-package org-roam
  :straight t
  :config
  (setq org-roam-directory (file-truename "~/org-roam"))
  (org-roam-db-autosync-mode))

(use-package avy
  :straight t
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-word-1)
   ("M-g g" . avy-goto-line))
  :config

  ;; Evil-mode-friendly bindings in normal mode
  (define-key evil-normal-state-map (kbd "s") #'avy-goto-char-timer)
  (define-key evil-normal-state-map (kbd "S") #'avy-goto-word-1)
  (define-key evil-normal-state-map (kbd "gs") #'avy-goto-line)
  (define-key evil-normal-state-map (kbd "gl") #'avy-goto-line)

  ;; Optional: visual state jump
  (define-key evil-visual-state-map (kbd "s") #'avy-goto-char-timer)

  (setq avy-timeout-seconds 0.3))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; context menu
   ("C-h B" . embark-bindings)) ;; list keybindings for current context
  :init
  ;; Optionally replace `describe-bindings` with `embark-bindings`
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; HFT Research Packages
;; ------------------------

;; Denote for filename-based note system
(use-package denote
  :straight t
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory (expand-file-name "~/notes/hft/"))
  (setq denote-known-keywords '("source" "concept" "lab" "experiment" "question" "glossary" "model" "empirical"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Use .org by default
  (setq denote-prompts '(title keywords))

  ;; Automatically rename Denote buffers
  (denote-rename-buffer-mode 1))

;; Citar for bibliography management
(use-package citar
  :straight t
  :config
  (setq citar-bibliography '("~/notes/hft/hft.bib"))
  (setq citar-library-paths '("~/notes/hft/pdfs/"))
  (setq citar-notes-paths '("~/notes/hft/sources/"))
  (setq citar-file-open-functions '(("pdf" . citar-file-open-external)))

  ;; Optional: use embark with citar
  (setq citar-at-point-function 'embark-act))

;; Citar-denote integration
(use-package citar-denote
  :straight t
  :after (citar denote)
  :config
  (citar-denote-mode)
  (setq citar-open-always-create-notes t))

;; PDF Tools for reading PDFs in Emacs
(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.1))

;; Python support
(use-package python-mode
  :straight t
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-indent-offset 4))

(use-package pyvenv
  :straight t
  :config
  (pyvenv-mode 1))

;; AUCTeX for LaTeX
(use-package tex
  :straight auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

;; Org-babel configuration for experiments
(with-eval-after-load 'org
  ;; Enable languages for org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (ocaml . t)
     (shell . t)
     (emacs-lisp . t)))

  ;; Don't ask for confirmation for safe languages
  (setq org-confirm-babel-evaluate
        (lambda (lang body)
          (not (member lang '("python" "ocaml" "shell" "emacs-lisp")))))

  ;; Better source block fontification
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  ;; Display inline images after babel execution
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; LaTeX preview settings
  (setq org-preview-latex-default-process 'dvipng)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))

;; Olivetti mode for focused reading
(use-package olivetti
  :straight t
  :config
  (setq olivetti-body-width 100))

;; Org-appear for clean LaTeX/links (enhance existing org-mode config)
(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autolinks t))

;; Load HFT Research Module
;; ------------------------
(add-to-list 'load-path (expand-file-name "~/.config/emacs/"))
(require 'hft-research)

;; Functions
;; ------------------------

(defun my/dired-this-buffer ()
  (interactive)
  (dired (file-name-directory buffer-file-name)))

(defun compile--make ()
  (interactive)
  (compile "make"))

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun reload-config ()
  (interactive)
  (load-file "~/.config/emacs/init.el"))

(defun open-config-file ()
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(defun open-line-above ()
  "Insert an empty line above the current one and move to it."
  (interactive)
  (beginning-of-line)
  (open-line 1))

(defun my/org-insert-inactive-timestamp-now ()
  "Insert an inactive Org timestamp like [2025-10-18 Sat 14:37] at point."
  (interactive)
  (require 'org)
  ;; ensure minute precision (no rounding)
  (let ((org-time-stamp-rounding-minutes '(0 1)))
    (org-insert-time-stamp (current-time) t t))) ; WITH-HM=t, INACTIVE=t

;; Theming
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'kaolin-mono-dark :no-confirm)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '())
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
