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


(let ((default-directory  "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(package-initialize)

(push '("marmalade" . "http://marmalade-repo.org/packages/") package-archives)
(push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-20171217.35")
  (require 'use-package))


(use-package slime-autoloads
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq-default inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy slime-repl)))


(use-package evil
             :config
             (evil-mode 1)
             (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
             (define-key evil-normal-state-map (kbd ",,") 'evil-buffer))


(use-package crosshairs
             :init
             (define-key evil-normal-state-map (kbd ",c") 'crosshairs-mode))


(use-package erc)
(use-package erc-services
  :init
  (setq erc-autojoin-channels-alist '((".*\\.freenode.net"
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
  (global-set-key "\C-x\C-f" 'helm-find-files))


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
  (setq bh/keep-clock-running nil)
  (setq org-clock-history-length 23)
  (setq org-clock-in-resume t)
  (setq org-clock-persist t)
  (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  (setq org-clock-into-drawer t)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  (setq org-clock-report-include-clocking-task t)
  (setq org-capture-templates
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


(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional
(setq jedi:get-in-function-call-timeout 200)


(add-to-list 'load-path 
  (expand-file-name "/usr/local/Cellar/ledger/3.1.1_6/share/emacs/site-lisp/")) 
(add-to-list 'auto-mode-alist '("\_ledger.txt$" . ledger-mode)) 
;; after setting up the load-path 
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)


;; make sure emacs uses environment variables from my shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;;

(load "~/.emacs.d/mpr-init.el")

(provide '.emacs)
;;; .emacs ends here
