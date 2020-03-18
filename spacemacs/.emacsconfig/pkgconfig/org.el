(setq org-agenda-files '("~/Dropbox/idiomatic.org"))

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

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((R . t)
;;    (ditaa . nil)
;;    (dot . nil)
;;    (emacs-lisp . t)
;;    (gnuplot . t)
;;    (haskell . nil)
;;    (latex . t)
;;    (ledger . t)
;;    (ocaml . nil)
;;    (octave . nil)
;;    (python . t)
;;    (ruby . nil)
;;    (screen . nil)
;;    (shell . t)
;;    (sql . nil)
;;    (sqlite . nil)
;;    (restclient . t)))

(set-face-foreground 'org-level-2 "cyan3")
(set-face-foreground 'org-level-3 "maroon1")

(setq org-babel-python-command "python3")

(spacemacs/set-leader-keys-for-major-mode 'org-mode "C-t" 'org-set-tags-command)

(setq org-hide-emphasis-markers t)

