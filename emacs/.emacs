;;; .emacs -- start emacs config process
;;; Commentary:
;;
;; Load packages and set package-specific configs.
;;
;; Packages:
;;
;; * package
;; * crosshairs
;; * erc
;; * erc-services
;; * helm
;; * helm-config
;; * powerline
;; * web-mode
;; * org
;; * org-bullets
;; * org-install
;; * evil
;; * org-id
;; * ox-latex
;; * evil-multiedit
;; * parse-csv


;;; Code:

(let ((default-directory  "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

; (push '("marmalade" . "http://marmalade-repo.org/packages/") package-archives)
; (push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-20171217.35")
  (require 'use-package))


(use-package slime-autoloads
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq-default inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy slime-repl)))


(defun evil-global-keymap (keychord function)
  (define-key evil-insert-state-map keychord function)
  (define-key evil-normal-state-map keychord function))


(use-package evil
  :config
  (evil-mode 1)
  (evil-global-keymap (kbd "C-h") 'evil-window-left)
  (evil-global-keymap (kbd "C-j") 'evil-window-down)
  (evil-global-keymap (kbd "C-k") 'evil-window-up)
  (evil-global-keymap (kbd "C-l") 'evil-window-right)
  (evil-global-keymap(kbd "C-c m") 'rotate-windows)
  (evil-global-keymap (kbd "C-c f") 'md-footnote)
  (evil-global-keymap [remap list-buffers] 'switch-to-buffer)
  (evil-global-keymap (kbd "C-x g") 'magit-status)
  (evil-global-keymap (kbd "C-x |") 'toggle-window-split)
  (evil-global-keymap (kbd "C-c j") 'insert-multiline-brace)
  (evil-global-keymap (kbd "C-c s") 'insert-src-block)
  (evil-global-keymap (kbd "C-c i") 'c-if)
  (evil-global-keymap (kbd "C-c h") 'help)
  (evil-global-keymap (kbd "M-.") 'nil)
  (define-key evil-normal-state-map (kbd "q") 'nil)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
  (define-key evil-insert-state-map (kbd "C-w") 'backward-kill-word)
  (define-key evil-motion-state-map (kbd "C-x C-k") 'kill-region)
  (define-key evil-motion-state-map (kbd "C-c C-k") 'kill-region)
  (evil-define-key 'normal org-mode-map
    (kbd "C-c C-x C-i") 'bh/punch-in
    (kbd "C-c C-x C-o") 'bh/punch-out)
  (evil-define-key 'normal gnus-group-mode-map
    (kbd "o") 'my-gnus-group-list-subscribed-groups))


(use-package crosshairs
             :init
             (define-key evil-normal-state-map (kbd ",c") 'crosshairs-mode))


(use-package erc)
(use-package erc-services
  :init
  (setq erc-autojoin-channels-alist '(("matthewroll.com:5000"
                                       "#emacs"
                                       "#reddit-cyberpunk"
                                       "#tcl"
                                       "##linux"
                                       "#archlinux-offtopic")))
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-timestamp-format "[%H:%M] ")
  (setq erc-fill-prefix "      + ")
  :config
  (erc-services-mode 1))


(use-package helm
  :init
  (defadvice helm-find-files (after find-file-sudo activate)
    "Find file as root if necessary."
    (unless (and buffer-file-name
                 (file-writable-p buffer-file-name))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
  (setq helm-file-globstar t)
  (global-set-key "\C-x\C-m" 'helm-M-x)
  (global-set-key "\C-c\C-m" 'helm-M-x)
  (global-set-key "\C-x\C-f" 'helm-find-files)
  (global-set-key (kbd "C-c g") 'helm-ag))


(use-package helm-projectile
  :init
  (define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)
  (setq projectile-completion-system 'helm)
  :config
  (projectile-mode)
  (helm-projectile-on))


(use-package helm-config
  :config
  (helm-mode 1))


(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-content-types-alist '())
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook))


(use-package org-id)

(load "~/.emacs.d/custom/bh-org.el")
(use-package org
  :init
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/refile.org")
  (add-hook 'org-mode-hook 'font-lock-mode)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c b") 'org-iswitchb)
  (global-set-key (kbd "C-c c") 'org-capture)
  (defun ck/org-confirm-babel-evaluate (lang body)
    (not (or (string= lang "latex")
             (string= lang "tcl")
             (string= lang "bash")
             (string= lang "ledger")
             (string= lang "python")
             (string= lang "emacs-lisp")
             (string= lang "shell")
             (string= lang "lisp"))))
  (setq org-confirm-babel-evaluate 'ck/org-confirm-babel-evaluate)
  (setq org-log-done 'time)
  (setq org-cycle-separator-lines 16)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files '("/Users/mroll/org/work.org"))
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "DarkOrange2" :weight bold)
                ("NEXT" :foreground "VioletRed2" :weight bold)
                ("DONE" :foreground "dark grey" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))
  (org-clock-persistence-insinuate)
  (defvar bh/keep-clock-running nil)
  (defvar org-clock-history-length 23)
  (defvar org-clock-in-resume t)
  (defvar org-clock-persist t)
  (defvar org-clock-in-switch-to-state 'bh/clock-in-to-next)
  (defvar org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  (defvar org-clock-into-drawer t)
  (defvar org-clock-out-remove-zero-time-clocks t)
  (defvar org-clock-out-when-done t)
  (defvar org-clock-persist-query-resume nil)
  (defvar org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  (defvar org-clock-report-include-clocking-task t)
  (defvar org-capture-templates
        (quote (("t" "todo" entry (file "~/org/refile.org")
                 "* TODO %?\n  %U\n\n  " :clock-in t :clock-resume t)
                ("r" "respond" entry (file "~/org/refile.org")
                 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                ("n" "note" entry (file "~/org/refile.org")
                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("j" "Journal" entry (file+datetree "~/org/diary.org")
                 "* %?\n%U\n" :clock-in t :clock-resume t)
                ("w" "org-protocol" entry (file "~/org/refile.org")
                 "* TODO Review %c\n%U\n" :immediate-finish t)
                ("m" "Meeting" entry (file "~/org/refile.org")
                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("p" "Phone call" entry (file "~/org/refile.org")
                 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                ("h" "Habit" entry (file "~/org/refile.org")
                 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
  (setq org-tag-alist (quote ((:startgroup)
                              ("TODO" . ?t)
                              ("NEXT" . ?n))))
  (setq org-fast-tag-selection-single-key (quote expert))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 10)))
  (setq org-startup-folded t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (ditaa . nil)
     (dot . nil)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . nil)
     (python . t)
     (ruby . nil)
     (screen . nil)
     (shell . t)
     (sql . nil)
     (sqlite . nil))))


(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-hide-emphasis-markers t))


(use-package org-install)


(use-package ox-latex
  :init
  (setq org-export-backends '(html ascii latex md))
  (setq org-list-demote-modify-bullet '(("1."  . "1)")
                                        ("1)"  . "1."))))

(use-package evil-multiedit
  :init
  (define-key evil-normal-state-map (kbd "C-n") 'evil-multiedit-match-and-next)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-multiedit-match-and-prev))


(use-package parse-csv)


(use-package epa-file
  :init
  (defvar epa-pinentry-mode 'loopback)
  :config
  (epa-file-enable))


(add-to-list 'load-path
  (expand-file-name "/usr/local/Cellar/ledger/3.1.1_6/share/emacs/site-lisp/"))
(add-to-list 'auto-mode-alist '("\_ledger.txt$" . ledger-mode))
;; after setting up the load-path
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)

(setq shell-file-name "/bin/bash")

(elpy-enable)
(defvar elpy-rpc-python-command "/usr/local/bin/python3")

(use-package request)

(use-package docker)

(use-package realgud)

(use-package dired-subtree
  :demand
  :bind
  (:map dired-mode-map
    ("<tab>" . mhj/dwim-toggle-or-open))
  :config
  (progn
    ;; Function to customize the line prefixes (I simply indent the lines a bit)
    (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))
    (setq dired-subtree-use-backgrounds nil)))

(use-package dired
  :ensure nil
  :config
  (progn
    (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls")
    (setq dired-listing-switches "-lXGh --group-directories-first")
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)))

(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;; make sure emacs uses environment variables from my shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;

(load "~/.emacs.d/mpr-secrets.el.gpg")

; depends on mpr-secrets
(load "~/.emacs.d/gnus.el")

; depends on gnus
(load "~/.emacs.d/mpr-init.el")

(setenv "WORKON_HOME" "/Users/mroll/.envs")
# GREENPHIRE PROJECT ENV
export DEBUG=true
export DATABASE_NAME=bpa-db
export DATABASE_USER=postgres
export DATABASE_PASSWORD=notApassword1!
export DATABASE_SERVICE_HOST=bpa-api-db

(use-package eyebrowse                  ; Easy workspaces creation and switching
  :ensure t
  :config
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-new-workspace t)

  (eyebrowse-mode t))

(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d61fc0e6409f0c2a22e97162d7d151dee9e192a90fa623f8d6a071dbf49229c6" "82fce2cada016f736dbcef237780516063a17c2436d1ee7f42e395e38a15793b" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "c158c2a9f1c5fcf27598d313eec9f9dceadf131ccd10abc6448004b14984767c" "715fdcd387af7e963abca6765bd7c2b37e76154e65401cd8d86104f22dd88404" "8ec2e01474ad56ee33bc0534bdbe7842eea74dccfb576e09f99ef89a705f5501" "0b6cb9b19138f9a859ad1b7f753958d8a36a464c6d10550119b2838cedf92171" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" default)))
 '(docker-global-mode t)
 '(frame-brackground-mode (quote dark))
 '(package-selected-packages
   (quote
    (eyebrowse heroku-theme badger-theme distinguished-theme hemisu-theme gruber-darker-theme gruvbox-theme dired-subtree js2-mode clues-theme alect-themes realgud docker request elpy helm-ag web-mode use-package typescript-mode slime parse-csv org-edna org-bullets org-blog org-babel-eval-in-repl magit ledger-mode jedi helm-projectile hamburg-theme green-phosphor-theme grandshell-theme geiser flycheck-pycheckers flycheck-gdc firecode-theme exec-path-from-shell evil-paredit evil-multiedit diminish diff-hl darkburn-theme dark-krystal-theme cyberpunk-theme crosshairs color-theme-sanityinc-solarized cherry-blossom-theme atom-one-dark-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
