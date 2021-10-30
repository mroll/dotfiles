;; -*- lexical-binding: t -*-
;;; package --- matt's emacs config

;;; Commentary:
;;

;;; Code:

;; For native-comp
(setq comp-speed 2)
(setq comp-deferred-compilation-deny-list '("\\(?:[^z-a]*-autoloads\\.el$\\)"))

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

(require 'seq)

;; --------------------------------------------------
;; Ladicle (https://ladicle.com/post/config/)
;; --------------------------------------------------

;; Default Encoding
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

;; Quiet Startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

(setq frame-title-format nil)
(setq ring-bell-function 'ignore)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash t)    ; Deleting files go to OS's trash folder
(setq make-backup-files nil)          ; Forbide to make backup files
(setq auto-save-default nil)          ; Disable auto save
(setq set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again
(setq track-eol t)			; Keep cursor at end of lines.
(setq line-move-visual nil)		; To be required by track-eol
(setq-default kill-whole-line t)	; Kill line including '\n'
(setq-default indent-tabs-mode nil)   ; use space
(defalias 'yes-or-no-p #'y-or-n-p)

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Recent files
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

(when (equal system-type 'darwin)
  (setq ns-auto-hide-menu-bar t)
  (setq ns-use-proxy-icon nil)
  (setq initial-frame-alist
     (append
      '((ns-transparent-titlebar . t)
        (ns-appearance . light)
        (vertical-scroll-bars . nil)
        (internal-border-width . 0)))))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-banner-logo-title "Yay Evil!"
        dashboard-items nil
        dashboard-set-footer nil))

(use-package posframe
  :straight t)

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

(use-package hl-line
  :straight t
  :hook
  (after-init . global-hl-line-mode))

(use-package highlight-indent-guides
  :straight t
  :diminish
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)) ; column

;; --------------------------------------------------
;; End (https://ladicle.com/post/config/)
;; --------------------------------------------------

;; Packages
;; ------------------------

(use-package add-node-modules-path
  :straight t
  :hook (rjsx-mode . add-node-modules-path))

(use-package all-the-icons
  :straight t)

(use-package ansi-color
  :straight t
  :config
  (defun display-ansi-colors ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package bind-map
  :straight t
  :after (forge yasnippet)
  :config

  (bind-map-set-keys yas-minor-mode-map
    "C-i" 'yas-insert-snippet)

  (bind-map-set-keys helm-rg-map
    "C-d" 'helm-rg--set-dir
    "TAB" 'helm-execute-persistent-action)

  (bind-map-set-keys helm-projectile-find-file-map
    "TAB" 'helm-execute-persistent-action)

  (bind-map my-base-leader-map
    :keys ("M-m")
    :evil-keys ("SPC")
    :evil-states (normal motion visual))

  (evil-define-key 'normal org-mode-map
    "o" '(lambda () (interactive) (evil-org-eol-call 'always-insert-item)))

  (bind-map my-org-mode-map
    :keys ("M-m m")
    :evil-keys ("SPC m" ",")
    :major-modes (org-mode))

  (bind-map-set-keys my-org-mode-map
    "c" 'org-capture
    "s" 'org-schedule
    "I" 'org-clock-in
    "O" 'org-clock-out
    "." 'org-time-stamp-inactive
    "," 'org-ctrl-c-ctrl-c
    "r" 'org-refile

    "ns" 'org-narrow-to-subtree
    "nw" 'widen
    )

  (evil-define-key 'normal org-capture-mode-map (kbd ", ,") 'org-capture-finalize)

  (bind-map my-scala-mode-map
    :keys ("M-m m")
    :evil-keys ("SPC m" ",")
    :major-modes (scala-mode))

  (bind-map-set-keys my-scala-mode-map
    "ss" 'sbt-start
    "sc" 'sbt-do-compile
    "sT" 'sbt/run-test-file
    "st" 'sbt/run-testcase-at-point)

  (bind-map-set-keys company-active-map
    "C-j" 'company-select-next
    "C-k" 'company-select-previous
    "C-w" 'backward-kill-word
    "RET" 'company-complete-selection)

  (bind-map-set-keys company-search-map
    "C-j" 'company-select-next
    "C-k" 'company-select-previous)

  ;; throws an error if we don't explicitly load org-agenda
  ;; in order to define 'org-agenda-mode-map
  (require 'org-agenda)
  (bind-map-set-keys org-agenda-mode-map
    "R" 'org-agenda-refile
    "r" 'org-agenda-refile)

  ;; Pull up custom agenda view w/ F1
  (global-set-key
    (kbd "<f1>")
    '(lambda (&optional arg)
       (interactive "P")
       (org-agenda arg ".")))

  (bind-map-set-keys my-base-leader-map
    "tt"   'vterm-toggle

    ;; M-x
    "SPC" 'helm-M-x

    ;; Application commands
    "aoc" 'org-capture
    "aoa" 'org-agenda
    ;; "am"  'mu4e

    ;; File commands
    "ff"  'helm-find-files
    "fs"  'save-buffer
    "fer" 'reload-config
    "fed" 'open-config-file
    "fj"  'dired-this-buffer

    ;; Window commands
    "wm" 'delete-other-windows
    "wd" 'delete-window
    "ws" 'split-window-below
    "wv" 'split-window-right
    "wl" 'windmove-right
    "wh" 'windmove-left
    "wk" 'windmove-up
    "wj" 'windmove-down
    "wF" 'make-frame
    "wo" 'other-frame

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
    "TAB" 'switch-to-last-buffer
    "bb"  'helm-buffers-list
    "bd"  'kill-this-buffer
    "ss"  'helm-swoop-without-pre-input
    "br"  'revert-buffer
    "bn"  'evil-buffer-new

    ;; Frame commands
    "tf" 'toggle-frame-fullscreen

    ;; Project commands
    "gs"  'magit-status
    "gr"  'mpr/refile-issue-from-project
    "pf"  'projectile-find-file
    "pp"  'projectile-switch-project
    "/"   'helm-projectile-rg
    "d/"  'mpr/helm-rg-from-dir
    "pv"  'projectile-run-vterm

    ;; Service commands
    "sm" 'prodigy

    ;; Syntax commands
    "el" 'flycheck-list-errors

    ;; Help
    "hk" 'describe-key
    "hf" 'describe-function
    "hv" 'describe-variable
    "hm" 'describe-mode

    ;; Emacs
    "qq" 'save-buffers-kill-emacs
    "qr" 'restart-emacs

    ;; Text
    "zx+" 'text-scale-increase
    )

  (bind-map-set-keys helm-find-files-map
    "C-h" 'helm-find-files-up-one-level
    "C-l" 'helm-execute-persistent-action
    "<tab>" 'helm-execute-persistent-action
    "C-j" 'helm-next-line
    "C-k" 'helm-previous-line)

  (bind-map-for-mode-inherit my-dired-mode-map my-base-leader-map
    :major-modes (dired-mode))

  (which-key-add-keymap-based-replacements my-base-leader-map
    "b" "buffer"
    "e" "flycheck"
    "f" "file"
    "g" "git"
    "h" "help"
    "p" "project"
    "q" "quit"
    "s" "search"
    "t" "frame"
    "w" "window")

  (global-set-key (kbd "C-;") 'comment-or-uncomment-region)
  )

(use-package projectile
  :straight t
  :config
  (projectile-mode +1))

(use-package company
  :straight t
  :after company-flow
  :init
  (global-company-mode)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.0)
  (setq company-tooltip-idle-delay 0.0)
  (setq company-tooltip-minimum-width 60)
  (setq company-tooltip-margin 2)
  :config
  (add-to-list 'company-backends 'company-flow))

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

(use-package evil-leader
  :straight t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")

  (define-key evil-normal-state-map (kbd "C-u") 'scroll-down-half-page)
  (define-key evil-normal-state-map (kbd "C-d") 'scroll-up-half-page))

(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config

  (setf evil-org-key-theme '(navigation insert textobjects additional))

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)

  )

(use-package eyebrowse
  :straight t
  :config
  (eyebrowse-mode t))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :straight t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
     doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-dracula t))

;; (use-package doom-themes
;;   :straight t
;;   :custom
;;   (doom-themes-enable-italic t)
;;   (doom-themes-enable-bold t)
;;   :custom-face
;;   ;; (vertical-bar   (doom-darken base5 0.4))
;;   ;; (doom-darken bg 0.4)
;;   :config
;;   (load-theme 'doom-dracula t)
;;   (doom-themes-neotree-config)
;;   (doom-themes-org-config)

;;   ;; Modeline
;;   (use-package doom-modeline
;;     :straight t
;;     :custom
;;     (doom-modeline-buffer-file-name-style 'truncate-with-project)
;;     (doom-modeline-icon t)
;;     (doom-modeline-major-mode-icon nil)
;;     (doom-modeline-minor-modes nil)
;;     :hook
;;     (after-init . doom-modeline-mode)
;;     :config
;;     (set-cursor-color "cyan")
;;     (line-number-mode 1)
;;     (column-number-mode 1)
;;     (doom-modeline-def-modeline 'main
;;       '(bar workspace-name window-number matches buffer-info remote-host buffer-position parrot selection-info)
;;       '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker ))))

(use-package flycheck-flow
  :straight t)

(use-package company-flow
  :straight t)

(use-package flycheck
  :straight t
 :after flycheck-flow
  :init
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(use-package helm
  :straight t
  :config
  (helm-mode 1)

  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "M-d") 'mpr/helm-rg--set-dir)

  (setq helm-split-window-in-side-p t))

(use-package helm-projectile
  :straight t
  :config
  (helm-projectile-on)

  (setq projectile-globally-ignored-directories '("node_modules"
                                               "target")))

(use-package helm-rg
  :straight t
  :config

  (setq helm-rg-thing-at-point nil)

  (defun mpr/helm-rg--set-dir ()
    "Set the directory in which to invoke ripgrep and search again."
    (interactive)
    (let ((pat helm-pattern))
      (helm-run-after-exit
       (lambda ()
         (let ((helm-rg--current-dir
                (expand-file-name
                 (projectile-complete-dir (projectile-acquire-root))
                 (projectile-acquire-root))))
           (helm-rg--do-helm-rg pat))))))

  (defun mpr/helm-rg-from-dir ()
    (interactive)
    (let ((helm-rg--current-dir
        (expand-file-name
         (projectile-complete-dir (projectile-acquire-root))
         (projectile-acquire-root))))
      (helm-rg helm-pattern)))

  (defun mpr/helm-rg--from-dir (dir)
    (let ((helm-rg--current-dir dir))
      (helm-rg helm-pattern)))

  (defmacro mpr/def-rg-from-dir-fn (name dir)
    `(defun ,name ()
       (interactive)
       (mpr/helm-rg--from-dir ,dir)))

  (mpr/def-rg-from-dir-fn mpr/rg-from-storyblanks-web "/Users/matt/src/storyblanks/web/")
  (mpr/def-rg-from-dir-fn mpr/rg-from-storyblanks-functions "/Users/matt/src/storyblanks/functions/"))

(use-package helm-swoop
  :straight t)

(use-package whitespace
  :straight t
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-basic-offset 2)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t))

(use-package lsp-ui
  :straight t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; Failing atm due to missing tsc-dyn dependency
;; Might be a Apple Silicon thing
;; (use-package tree-sitter
;;   :straight t)

;; (use-package tree-sitter-langs
;;   :straight t)

(use-package magit
  :straight t
  :config

  (setq magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)

  (evil-define-key 'normal with-editor-mode-map (kbd ",,") 'with-editor-finish)
  (evil-define-key 'motion with-editor-mode-map (kbd ",,") 'with-editor-finish))

(use-package forge
  :straight t
  :after magit
  :config
  (setq auth-sources '("~/.authinfo")))

(use-package s
  :straight t)

;; (use-package mu4e
;;   :straight ( :host github
;;               :repo "djcb/mu"
;;               :branch "master"
;;               :files ("mu4e/*")
;;               :pre-build (("./autogen.sh") ("make")))
;;   :custom   (mu4e-mu-binary (expand-file-name "mu/mu" (straight--repos-dir "mu")))
;;   :hook (evil-collection-setup . (lambda (&rest a)
;;            (evil-define-key 'normal mu4e-headers-mode-map
;;              (kbd "a") 'mu4e-headers-mark-for-archive)))
;;   :config
;;
;;   (set-face-attribute 'variable-pitch nil :height 160)
;;   (set-face-attribute 'mu4e-highlight-face nil :inherit 'default)
;;
;;   (setq mail-user-agent 'mu4e-user-agent)
;;
;;   ;; default
;;   (setq mu4e-maildir "~/.mail/pm")
;;   (setq mu4e-drafts-folder "/Drafts")
;;   (setq mu4e-sent-folder   "/Sent")
;;   (setq mu4e-trash-folder  "/Trash")
;;   (setq mu4e-refile-folder  "/Archive")
;;
;;   (setq mu4e-headers-fields
;;   '( (:human-date       .   12)
;;      (:flags            .    6)
;;      (:mailing-list     .   10)
;;      (:from             .   22)
;;      (:thread-subject   .   nil)))
;;
;;   (setq mu4e-headers-full-search nil)
;;   (setq mu4e-headers-result-limit 1000)
;;   (setq message-kill-buffer-on-exit t)
;;
;;   (setq mu4e-view-show-images t)
;;
;;   ;; (mu4e-alert-set-default-style 'notifier)
;;   ;; (mu4e-alert-enable-mode-line-display)
;;
;;   ;; This allows me to use 'helm' to select mailboxes
;;   (setq mu4e-completing-read-function 'completing-read)
;;   ;; Why would I want to leave my message open after I've sent it?
;;   (setq message-kill-buffer-on-exit t)
;;   ;; Don't ask for a 'context' upon opening mu4e
;;   (setq mu4e-context-policy 'pick-first)
;;   ;; Don't ask to quit... why is this the default?
;;   (setq mu4e-confirm-quit nil)
;;
;;   (setq user-mail-address "mproll@pm.me")
;;   (setq smtpmail-default-smtp-server "127.0.0.1"
;;   smtpmail-smtp-server "127.0.0.1"
;;   smtpmail-smtp-service 1025)
;;   (setq message-send-mail-function 'smtpmail-send-it)
;;
;;   (setq mu4e-update-interval 300)
;;   (setq mu4e-view-show-addresses 't)
;;
;;   (setq mu4e-get-mail-command "offlineimap -o")
;;
;;   (setq mu4e-maildir-shortcuts
;;   '( ("/INBOX"                       . ?i)
;;      ("/Sent"                        . ?s)
;;      ("/Archive"                     . ?a)
;;      ("/Trash"                       . ?T)
;;      ("/Folders.newsletters"         . ?n)))
;;
;;   (setq mu4e-headers-fields
;;   '( (:human-date       .   12)
;;      (:flags            .    6)
;;      (:mailing-list     .   10)
;;      (:from             .   22)
;;      (:thread-subject   .   nil)))
;;
;;   (setq mu4e-view-prefer-html t)
;;
;;   ;; Mark as read and archive
;;   (add-to-list 'mu4e-marks
;;          '(archive
;;      :char       "A"
;;      :prompt     "Archive"
;;      :show-target (lambda (target) "archive")
;;      :action      (lambda (docid msg target)
;;         ;; must come before proc-move since retag runs
;;         ;; 'sed' on the file
;;         (mu4e-action-retag-message msg "-/Inbox")
;;         (mu4e~proc-move docid "/Archive" "+S-u-N"))))
;;
;;   (mu4e~headers-defun-mark-for archive)
;;
;;   )

(use-package org
  :straight t
  :config
  (require 'org-capture)
  (require 'org-tempo)

  (setq mpr/org-dir "~/org/")

  (setq org-agenda-files `(,mpr/org-dir))
  (setq org-hide-emphasis-markers t)
  (setq org-use-speed-commands t)
  (setq org-directory mpr/org-dir)
  (setq org-default-notes-file (concat mpr/org-dir "inbox.org"))
  (setq org-agenda-window-setup 'only-window)

  (setq mpr/org-agenda-directory mpr/org-dir)

  (add-to-list 'org-capture-templates
       `("i" "inbox" entry (file ,(concat mpr/org-agenda-directory "inbox.org"))
         "* TODO %?\n  %U"))

  (setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

  ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 23)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)

  (setq org-clock-out-remove-zero-time-clocks t)

  (setq org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)
  ;; Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  ;; Include current clocking task in clock reports
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
  (quote ((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d)")
    (sequence "HOLD(h)"))))

  (setq org-todo-keyword-faces
  (quote (("TODO" :foreground "orange" :weight bold)
    ("NEXT" :foreground "#32ed5e")
    ("DONE" :foreground "gray38" :weight bold)
    ("HOLD" :foreground "magenta" :weight bold)
    ("CANCELLED" :foreground "gray38" :weight bold))))

  (setq mpr/org-agenda-todo-view
        (if (string-equal (system-name) "hogwarts.local")
            `("." "Agenda"
              ((agenda ""
                       ((org-agenda-span 'day)
                        (org-deadline-warning-days 365)))
               (todo "TODO"
                     ((org-agenda-overriding-header "To Refile")
                      (org-agenda-files '(,(concat mpr/org-agenda-directory "inbox.org")))))
               (todo '("NEXT" "TODO")
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-files '(,(concat mpr/org-agenda-directory "projects.org")))))
               nil))

          `("." "Agenda"
            ((agenda ""
                     ((org-agenda-span 'day)
                      (org-deadline-warning-days 365)))
             (todo '("NEXT" "TODO")
                   ((org-agenda-overriding-header "To Refile")
                    (org-agenda-files '(,(concat mpr/org-agenda-directory "inbox.org")))))
             (todo '("NEXT" "TODO")
                   ((org-agenda-overriding-header "Idiomatic")
                    (org-agenda-files '(,(concat mpr/org-agenda-directory "idiomatic.org")))))
             nil))))

  (setq org-agenda-custom-commands `(,mpr/org-agenda-todo-view))

  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(("~/org/next.org" :level . 0)
           ("~/org/idiomatic.org" :maxlevel . 5)
           ("~/org/projects.org" :maxlevel . 5)))

  (org-clock-persistence-insinuate)

  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  (defun ck/org-confirm-babel-evaluate (lang body)
    (not (or (string= lang "latex")
       (string= lang "tcl")
       (string= lang "bash")
       (string= lang "ledger")
       (string= lang "python")
       (string= lang "emacs-lisp")
       (string= lang "shell")
       (string= lang "lisp"))))
  (setq-default org-confirm-babel-evaluate 'ck/org-confirm-babel-evaluate)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (ditaa . nil)
     (dot . nil)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . t)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . nil)
     (python . t)
     (ruby . nil)
     (screen . nil)
     (shell . t)
     (sql . nil)
     (sqlite . nil)))

  (evil-define-key 'normal org-mode-map "t" 'org-todo)

  (evil-define-key 'normal org-mode-map (kbd "SPC h i") 'org-insert-heading)
  (evil-define-key 'normal org-mode-map (kbd "SPC h s") 'org-insert-subheading)

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

;; (use-package org-roam
;;       :straight t
;;       :ensure t
;;       :hook
;;       (after-init . org-roam-mode)
;;       :custom
;;       (org-roam-directory "~/org/notes")
;;       :bind (:map org-roam-mode-map
;;         (("C-c n l" . org-roam)
;;          ("C-c n f" . org-roam-find-file)
(use-package add-node-modules-path
  :straight t
  :hook (rjsx-mode . add-node-modules-path))

(use-package company
  :straight t
  :after company-flow
  :init
  (global-company-mode)
  (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.0)
  (setq company-tooltip-idle-delay 0.5)
  :config
  (add-to-list 'company-backends 'company-flow))

(use-package flycheck
  :straight t
  :after flycheck-flow
  :init
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(use-package lsp-mode
  :straight t
  :hook
  (js2-mode . lsp)
  (rjsx-mode . lsp)
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package prettier-js
  :straight t
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode 'prettier-js-mode)
  (add-hook 'tide-mode 'prettier-js-mode))

(use-package rjsx-mode
  :straight t
  :mode ("\\.jsx\\'" "\\.tsx\\'"))

(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))

(use-package smartparens
  :straight t
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

(use-package tide
  :straight t
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  )

;; (use-package typescript-mode
;;   :straight t
;;   (add-hook 'typescript-mode #'smartparens-mode))

(use-package web-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package popwin
  :straight t
  :config
  (popwin-mode 1))

(use-package flow-minor-mode
  :straight t
  :config
  (add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
  (add-hook 'rjsx-mode-hook 'flow-minor-enable-automatically))

;; TODO: work in auto layouts https://github.com/kiwanami/emacs-window-layout
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
  :straight t
  :config
  (projectile-mode +1))

(use-package restart-emacs
  :straight t)

(use-package rjsx-mode
  :straight t
  :mode ("\\.jsx\\'" "\\.tsx\\'"))

(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)

  (ert-deftest sbt/get-testonly-file_filename_testOnlyFile ()
    (should
     (string= (sbt/get-testonly-file "testSpec") "testOnly *testSpec")))

  (defun sbt/get-testonly-file (&optional file)
    "Return FILE formatted in a sbt testOnly command."
    (--> (or file (file-name-base))
      (format "testOnly *%s" it)))

  (defun sbt/run-test-file (&optional file)
    (interactive)
    (sbt-command (sbt/get-testonly-file file)))

  (defun second (l)
    (if l (cadr l)))


  (defun sbt/get-testcase-name ()
    "Get Scala test case nearby point."
    (save-excursion
      (let* ((line (thing-at-point 'line t))
             (on-testcase-p (and (s-contains? "\"" line)
                                 (s-contains? "{\n" line)))
             (get-testcase-name (lambda (l)
                                  (--> l
                                    (s-split "\"" it)
                                    reverse
                                    second))))
        (if on-testcase-p
            (funcall get-testcase-name line)
          (progn
            (search-backward "{\n")
            (funcall get-testcase-name (thing-at-point 'line t)))))))

  (defun sbt/run-testcase-at-point ()
    "Run Scala test case at point."
    (interactive)
    (--> (sbt/get-testonly-file)
      (format "%s -- -z \"%s\"" it (sbt/get-testcase-name))
      sbt-command))


  )

(use-package scala-mode
  :straight t
  :interpreter
  ("scala" . scala-mode)
  :hook (scala-mode . (lambda () electric-pair-mode)))

(use-package smartparens
  :straight t
  :config
  (add-hook 'js-mode-hook #'smartparens-mode)
  (add-hook 'scala-mode-hook #'smartparens-mode))

(use-package sublime-themes
  :straight t
  :config
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes"))

;; this is cute but it gets in the way of minibuffer messages
;; (use-package symon
;;   :straight t
;;   :config
;;   (symon-mode))


(use-package tide
  :straight t
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  ;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  )

(use-package typescript-mode
  :straight t)

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(use-package vterm
  :straight t)

(use-package multi-vterm
  :straight t
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "TAB")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume)


  (global-set-key (kbd "C-v") 'vterm-yank)
  (evil-define-key 'normal vterm-mode-map "P" 'vterm-yank)
  (evil-define-key 'normal vterm-mode-map "p" 'vterm-yank))

(use-package vterm-toggle
  :straight t
  :config
  (define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(use-package web-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package which-key
  :straight t
  :after (bind-map)
  :config
  (setq which-key-idle-delay 0.2)

  (which-key-mode))

(use-package writeroom-mode
  :straight t)

(use-package yasnippet
  :straight t
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-i") #'yas-insert-snippet)

  (setq yas-snippet-dirs '("~/.emacs.d/snippets/"
                           "~/Dropbox/dotfiles/emacs/snippets/"))

  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t)


;; Functions
;; ------------------------

(defun eval-next-sexp ()
  (interactive)
  (save-excursion
    (forward-sexp)
    (eval-last-sexp nil)))

(defun eval-surrounding-sexp (levels)
  (interactive "p")
  (save-excursion
    (up-list (abs levels))
    (eval-last-sexp nil)))

(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.el"))

(defun reload-config ()
  (interactive)
  (load-file "~/.emacs.el"))

(defun dired-this-buffer ()
  (interactive)
  (find-file default-directory))

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun scroll-up-half-page ()
  (interactive)
  (scroll-up-command
   (truncate (/ (window-total-height) 2))))

(defun scroll-down-half-page ()
  (interactive)
  (scroll-down-command
   (truncate (/ (window-total-height) 2))))

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

;; Hooks
;; ------------------------

(add-hook 'text-mode-hook 'turn-on-auto-fill)


;; Variables
;; ------------------------

(setq helm-input-idle-delay 0)

(setq default-frame-alist '((font . "Jetbrains Mono")))
(setq-default line-spacing .1)

(scroll-bar-mode -1)
(menu-bar-mode   -1)
(tool-bar-mode   -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist '(("." . "~/.emacs.d/.backups/")))

;; (when window-system
;;   (set-frame-size (selected-frame) 110 98))

(add-to-list
 'default-frame-alist'(ns-transparent-titlebar . t))
(add-to-list
 'default-frame-alist'(ns-appearance . light))


;; (load-theme 'doom-one-light t)

(set-face-attribute 'default nil :height 120)

(setq vc-follow-symlinks t)

;; writeroom mode
(setq-default markdown-header-scaling t)
(setq-default markdown-hide-markup t)
(setq-default writeroom-maximize-window nil)
(setq-default writeroom-width 70)

(defun writing-mode ()
  (interactive)
  (setq buffer-face-mode-face '(:family "San Francisco" :height 172))
  (buffer-face-mode)
  (linum-mode 0)
  (writeroom-mode 1)
  (blink-cursor-mode)
  (visual-line-mode 1)
  (setq truncate-lines nil)
  (setq line-spacing 5)
  (setq global-hl-line-mode nil))
;; (add-hook 'markdown-mode-hook 'writing-mode)

(defun blogging-mode ()
  (interactive)
  (setq buffer-face-mode-face '(:family "San Francisco" :height 120))
  (visual-line-mode)
  (visual-fill-column-mode)
  (setq visual-fill-column-width 80)
  (org-indent-mode)
  (variable-pitch-mode)
  (auto-fill-mode -1))
(setq org-image-actual-width nil)

(defun mpr/refile-issue-from-project ()
  (interactive)
  (let* ((repo (forge-get-repository
                (forge-read-repository "Choose a project")))
         (issues (forge-ls-issues repo)))
    (mpr/helm-refile-issue issues)))

(defun mpr/helm-refile-issue (issues)
  (helm :sources
        (helm-build-sync-source "issue"
          :candidates
          (lambda ()
            (mapcar (lambda (issue)
                      (cons (oref issue title) issue))
                    issues))
          :action (list (cons "Refile issue" (lambda (candidate)
                                               (mpr/refile-issues
                                                (helm-marked-candidates))))))
        :buffer "*helm issues*"
        :prompt "issue: "))

(defun mpr/refile-issues (issues)
  (let ((rfloc (org-refile-get-location)))
    (mapc (lambda (issue)
            (mpr/refile-issue issue rfloc))
          issues)))

(defun mpr/refile-issue (issue &optional rfloc-arg)
  (let ((rfloc (or rfloc-arg (org-refile-get-location))))
    (with-temp-buffer
      (org-mode)
      (insert (format "* TODO #%s %s"
                      (oref issue number)
                      (oref issue title)))
      (org-refile nil nil rfloc))))


(define-key ctl-x-map "\C-i"
  #'mpr/ispell-word-then-abbrev)

(defun mpr/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun mpr/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (mpr/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (mpr/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212337" "#ff757f" "#c3e88d" "#ffc777" "#82aaff" "#c099ff" "#b4f9f8" "#c8d3f5"])
 '(custom-safe-themes
   '("4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "fd22c8c803f2dac71db953b93df6560b6b058cb931ac12f688def67f08c10640" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "56d10d2b60685d112dd54f4ba68a173c102eacc2a6048d417998249085383da1" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "0685ffa6c9f1324721659a9cd5a8931f4bb64efae9ce43a3dba3801e9412b4d8" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "e3c64e88fec56f86b49dcdc5a831e96782baf14b09397d4057156b17062a8848" "fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "f4876796ef5ee9c82b125a096a590c9891cec31320569fc6ff602ff99ed73dca" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "6084dce7da6b7447dcb9f93a981284dc823bab54f801ebf8a8e362a5332d2753" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "6c9cbcdfd0e373dc30197c5059f79c25c07035ff5d0cc42aa045614d3919dab4" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "35c096aa0975d104688a9e59e28860f5af6bb4459fd692ed47557727848e6dfe" "f490984d405f1a97418a92f478218b8e4bcc188cf353e5dd5d5acd2f8efd0790" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" default))
 '(fci-rule-color "#444a73")
 '(flycheck-javascript-flow-args nil)
 '(helm-completion-style 'emacs)
 '(helm-minibuffer-history-key "M-p")
 '(jdee-db-active-breakpoint-face-colors (cons "#161a2a" "#82aaff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#161a2a" "#c3e88d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#161a2a" "#444a73"))
 '(objed-cursor-color "#ff757f")
 '(pdf-view-midnight-colors (cons "#c8d3f5" "#212337"))
 '(rustic-ansi-faces
   ["#212337" "#ff757f" "#c3e88d" "#ffc777" "#82aaff" "#c099ff" "#b4f9f8" "#c8d3f5"])
 '(vc-annotate-background "#212337")
 '(vc-annotate-color-map
   (list
    (cons 20 "#c3e88d")
    (cons 40 "#d7dd85")
    (cons 60 "#ebd27e")
    (cons 80 "#ffc777")
    (cons 100 "#ffb76e")
    (cons 120 "#ffa866")
    (cons 140 "#ff995e")
    (cons 160 "#ea9993")
    (cons 180 "#d599c9")
    (cons 200 "#c099ff")
    (cons 220 "#d58dd4")
    (cons 240 "#ea81a9")
    (cons 260 "#ff757f")
    (cons 280 "#d06a7c")
    (cons 300 "#a15f79")
    (cons 320 "#725476")
    (cons 340 "#444a73")
    (cons 360 "#444a73")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
