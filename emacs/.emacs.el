;;; package --- matt's emacs config

;;; Commentary:
;;


;;; Code:

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

;; Packages
;; ------------------------

(use-package add-node-modules-path
  :straight t
  :hook (rjsx-mode . add-node-modules-path))

(use-package all-the-icons
  :straight t)

(use-package bind-map
  :straight t
  :config

  (bind-map my-base-leader-map
    :keys ("M-m")
    :evil-keys ("SPC")
    :evil-states (normal motion visual))

  (bind-map my-org-mode-map
    :keys ("M-m m")
    :evil-keys ("SPC m" ",")
    :major-modes (org-mode))

  (bind-map-set-keys my-org-mode-map
    "c" 'org-capture
    "s" 'org-schedule
    "I" 'org-clock-in
    "O" 'org-clock-out
    "." 'org-time-stamp-inactive)

  ;; throws an error if we don't explicitly load org-agenda
  ;; in order to define 'org-agenda-mode-map
  (require 'org-agenda)
  (bind-map-set-keys org-agenda-mode-map
    "R" 'org-agenda-refile)

  (bind-map-set-keys my-base-leader-map
    ;; M-x
    "SPC" 'helm-M-x

    ;; Application commands
    "aoc" 'org-capture
    "aoa" 'org-agenda

    ;; File commands
    "ff" 'helm-find-files
    "fs" 'save-buffer
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

    ;; Buffer commands
    "TAB" 'switch-to-last-buffer
    "bb"  'helm-buffers-list
    "bd"  'kill-this-buffer
    "ss"  'helm-swoop-without-pre-input
    "br"  'revert-buffer

    ;; Frame commands
    "tF" 'toggle-frame-fullscreen

    ;; Project commands
    "gs"  'magit-status
    "pf"  'projectile-find-file
    "pp"  'projectile-switch-project
    "/"   'helm-projectile-rg

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
    "qr" 'restart-emacs
    )

  (bind-map-set-keys helm-find-files-map
    "C-h" 'helm-find-files-up-one-level
    "C-l" 'helm-execute-persistent-action
    "TAB" 'helm-execute-persistent-action
    "C-j" 'helm-next-line
    "C-k" 'helm-previous-line))

(use-package company
  :straight t
  :init
  (global-company-mode)
  (setq company-tooltip-align-annotations t))

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
	   '(magit calc calendar dired flycheck company))
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
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode))

(use-package helm
  :straight t
  :config
  (helm-mode 1)

  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)

  (setq helm-split-window-in-side-p t))

(use-package helm-projectile
  :straight t
  :config
  (helm-projectile-on)

  (setq projectile-globally-ignored-directories '("node_modules"
						  "target")))

(use-package helm-rg
  :straight t)

(use-package helm-swoop
  :straight t)

(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-basic-offset 2)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t))

(use-package lsp-mode
  :straight t
  :hook
  (js2-mode . lsp)
  (rjsx-mode . lsp)
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package magit
  :straight t)

(use-package org
  :straight t
  :config
  (setq org-agenda-files '("~/Dropbox/org/idiomatic.org"
			   "~/Dropbox/org/projects.org"
			   "~/Dropbox/org/next.org"))
  (setq org-hide-emphasis-markers t)
  (setq org-use-speed-commands t)
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file "~/Dropbox/org/inbox.org")

  (setq matt/org-agenda-directory "~/Dropbox/org/")
  (add-to-list 'org-capture-templates
	     `("i" "inbox" entry (file ,(concat matt/org-agenda-directory "inbox.org"))
	       "* TODO %?"))

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

  (setq matt/org-agenda-todo-view
	`("." "Agenda"
	  ((agenda ""
		   ((org-agenda-span 'day)
		    (org-deadline-warning-days 365)))
	   (todo "TODO"
		 ((org-agenda-overriding-header "To Refile")
		  (org-agenda-files '(,(concat matt/org-agenda-directory "inbox.org")))))
	   (todo '("NEXT" "TODO")
		 ((org-agenda-overriding-header "Idiomatic")
		  (org-agenda-files '(,(concat matt/org-agenda-directory "idiomatic.org")))))
	   (todo "TODO"
		 ((org-agenda-overriding-header "Projects")
		  (org-agenda-files '(,(concat matt/org-agenda-directory "projects.org")))
		  ))
	   (todo "TODO"
		 ((org-agenda-overriding-header "One-off Tasks")
		  (org-agenda-files '(,(concat matt/org-agenda-directory "next.org")))
		  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	   nil)))

  (setq org-agenda-custom-commands `(,matt/org-agenda-todo-view))

  (setq org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(("next.org" :level . 0)
			     ("idiomatic.org" :maxlevel . 5)
			     ("projects.org" :maxlevel . 5)))

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

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-roam
      :straight t
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Dropbox/org/notes")
      :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n f" . org-roam-find-file)
	       ("C-c n g" . org-roam-graph))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert))
	      (("C-c n I" . org-roam-insert-immediate))))

(use-package popwin
  :straight t
  :config
  (popwin-mode 1))

(use-package prettier-js
  :straight t
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))

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
    :name "idiomatic-enduser"
    :command "~/src/product/activator"
    :args '("-d" "-J-Xmx2048M" "-J-XX:MaxMetaspaceSize=512M" "enduser/run"
	    "-Dconfig.resource=enduser.matt.application.conf" "-Dhttp.port=9000"
	    "-Duser.timezone=UTC" "-Dlogger.resource=enduser.matt.logback.xml"
	    "-Dlogback.debug=true")
    :port 9000
    :cwd "~/src/product"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "idiomatic-engine"
    :command "~/src/product/activator"
    :args '("-d" "-J-Xmx6000M" "-J-XX:MaxMetaspaceSize=512M" "engine/run"
	    "-Dconfig.resource=engine.matt.application.conf" "-Dhttp.port=9000"
	    "-Duser.timezone=UTC" "-Dlogger.resource=engine.matt.logback.xml"
	    "-Dlogback.debug=true")
    :port 9000
    :cwd "~/src/product"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "idiomatic-internal"
    :command "~/src/product/activator"
    :args '("-d" "-J-Xmx2048M" "-J-XX:MaxMetaspaceSize=512M" "internal/run"
	    "-Dconfig.resource=internal.matt.application.conf" "-Dhttp.port=8080"
	    "-Duser.timezone=UTC" "-Dlogger.resource=internal.matt.logback.xml"
	    "-Dlogback.debug=true")
    :port 8080
    :cwd "~/src/product"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "idiomatic-jobrunner"
    :command "~/src/product/activator"
    :args '("-d" "-J-Xmx2048M" "-J-XX:MaxMetaspaceSize=512M" "jobrunner/run"
	    "-Dconfig.resource=jobrunner.matt.application.conf" "-Dhttp.port=9001"
	    "-Duser.timezone=UTC" "-Dlogger.resource=jobrunner.prod.logback.xml"
	    "-Dlogback.debug=true")
    :port 9001
    :cwd "~/src/product"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(use-package projectile
  :straight t
  :config
  (projectile-mode +1))

(use-package restart-emacs
  :straight t)

(use-package rjsx-mode
  :straight t
  :mode "\\.jsx\\'")

(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package scala-mode
  :straight t
  :interpreter
  ("scala" . scala-mode))

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


(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

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

  (which-key-mode))

;; Functions
;; ------------------------

(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun reload-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

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


;; Hooks
;; ------------------------

(add-hook 'text-mode-hook 'turn-on-auto-fill)


;; Variables
;; ------------------------

(setq default-frame-alist '((font . "Hack")))

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

(load-theme 'doom-moonlight)

(set-face-attribute 'default nil :height 140)

;; Join the #emacs and #erc channels whenever connecting to Freenode.
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#erc")))

;; Rename server buffers to reflect the current network name instead
;; of SERVER:PORT (e.g., "freenode" instead of "irc.freenode.net:6667").
;; This is useful when using a bouncer like ZNC where you have multiple
;; connections to the same server.
(setq erc-rename-buffers t)

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)


;; Custom
;; -----------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "6084dce7da6b7447dcb9f93a981284dc823bab54f801ebf8a8e362a5332d2753" "188fed85e53a774ae62e09ec95d58bb8f54932b3fd77223101d036e3564f9206" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "35c096aa0975d104688a9e59e28860f5af6bb4459fd692ed47557727848e6dfe" "f490984d405f1a97418a92f478218b8e4bcc188cf353e5dd5d5acd2f8efd0790" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" default))
 '(helm-completion-style 'emacs)
 '(helm-minibuffer-history-key "M-p"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
