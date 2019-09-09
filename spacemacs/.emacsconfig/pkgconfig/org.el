(setq org-agenda-files '("~/Dropbox/org/money/money.org"
                         "~/Dropbox/org/project"
                         "~/Dropbox/org/today.org"
                         "~/Dropbox/org/agenda-refile-beorg.org"))

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
   (sqlite . nil)
   (restclient . t)))

(set-face-foreground 'org-level-3 "maroon1")

(setq org-babel-python-command "python3")

;; (add-to-list 'org-latex-classes
;;              '("book"
;;                "\\documentclass{book}"
;;                ("\\part{%s}" . "\\part*{%s}")
;;                ("\\chapter{%s}" . "\\chapter*{%s}")
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
