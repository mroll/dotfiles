;;; mpr-init.el -- emacs customization
;;
;; Author: matt roll
;;

;;; Commentary:

;;; Code:

;; -------------------------------------------------------------------
;; functions
;; -------------------------------------------------------------------

(defun comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
; --------------------------------------------------

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

; --------------------------------------------------

(defun buffer-substring-to-lines (start end delim)
  "Splits the region on delim and joins back on newlines"
  (interactive "r\nsDelimiter: ")
  (let ((string (buffer-substring start end)))
    (kill-new
     (mapconcat 'identity (split-string string delim nil "\s") "\n"))))

(defun join-region (start end delim)
  "Joins the lines in the region by delim"
  (interactive "r\nsDelimiter: ")
  (let ((string (buffer-substring start end)))
    (kill-new
     (mapconcat 'identity (split-string string "\n" nil "\s") delim))))
  
(defun output-files-to-buffer (pathstring regexp)
  "Use regexp to match files and insert them into the buffer at point."
  (interactive "sEnter paths as a space-separated list: \nsEnter regexp: ")
  (let ((paths (split-string pathstring "\s")))
    (insert (mapconcat 'identity (loop for path in paths
                                       collect (shell-command-to-string (format "ls %s | grep \"%s\"" path regexp)))
                       "\n"))))

(defun write-multiline-brace-expr (expr)
  (insert expr)
  (insert " {
}")
  (left-char)
  (c-indent-line-or-region)
  (evil-open-above 1))
                                        
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(defun c-for-loop-upto (var upto)
  "write boilerplate for C style for loop"
  (interactive "siter var: \nsupto: ")
  (write-multiline-brace-expr (format "for (int %s=0; %s<%s; %s++)" var var upto var)))

(defun tcl-proc (name args)
  "write boilerplate for a tcl proc definition"
  (interactive "sname: \nsargs: ")
  (write-multiline-brace-expr (format "proc %s { %s }" name args)))

(defun insert-multiline-brace ()
  (interactive)
  (insert " {
}")
  (left-char)
  (c-indent-line-or-region)
  (evil-open-above 1))

(defun insert-src-block (lang options)
  (interactive "slang: \nsoptions: \n")
  (insert (format "#+begin_src %s %s\n#+end_src" lang options))
  (left-char)
  (c-indent-line-or-region)
  (evil-open-above 1))

(defun c-if (condition)
  (interactive "sCondition: ")
  (insert (format "if (%s)" condition))
  (insert-multiline-brace))

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

(defun what-face ()
  (interactive)
  (message (face-font (or (get-char-property (point) 'face)
                          'default))))

(defun mpr/erc-connect ()
  (erc :server "matthewroll.com" :port 5000 :nick "le4fy" :password mpr/znc-pass))
; (mpr/erc-connect)

;; -------------------------------------------------------------------


;; -------------------------------------------------------------------
;; mode hooks
;; -------------------------------------------------------------------

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'tcl-mode-hook 'comment-auto-fill)

(add-hook 'lisp-mode-hook
	   (lambda ()
	     (auto-fill-mode 1)
	     (set (make-local-variable 'fill-nobreak-predicate)
		  (lambda ()
		    (not (eq (get-text-property (point) 'face)
			     'font-lock-comment-face))))))

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;; keybindings
;; -------------------------------------------------------------------

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-c h") 'help)

; move between panes w/ Ctrl+<vi arrow keys>
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

; forgive fat-fingering buffer switch. run ibuffer explicitly
(define-key global-map [remap list-buffers] 'switch-to-buffer)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x |") 'toggle-window-split)


(define-key global-map "\C-cj" 'insert-multiline-brace)
(define-key global-map "\C-cs" 'insert-src-block)
(define-key global-map "\C-ci" 'c-if)

(define-key global-map "\C-c\C-x\C-i" 'bh/punch-in)
(define-key global-map "\C-c\C-x\C-o" 'bh/punch-out)

(define-key gnus-group-mode-map
  ;; list all the subscribed groups even they contain zero un-read messages
  (kbd "o") 'my-gnus-group-list-subscribed-groups)

;; -------------------------------------------------------------------


;; -------------------------------------------------------------------
;; config variables
;; -------------------------------------------------------------------

; aesthetics
; --------------------------------------------------
(setq default-frame-alist '((width . 170)
                            (height . 50)
                            (menu-bar-lines . 0)))

(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))

(setq column-number-mode t)

; turn of scroll bar, tool bar, and menu bar
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(set-face-attribute 'org-agenda-date-today nil :foreground "gray29" :weight 'bold :family "Courier New")
(set-face-attribute 'org-level-1 nil :inherit 'variable-pitch :foreground "#cb4b16" :height 1.3 :family "Courier New")
(set-face-attribute 'org-done nil :foreground "gray29" :weight 'bold :family "Courier New")
(set-face-attribute 'org-level-2 nil :inherit 'variable-pitch :foreground "#0EEB87" :height 1.2 :family "Courier New")
(set-face-attribute 'org-level-3 nil :inherit 'variable-pitch :foreground "#268bd2" :height 1.15 :family "Courier New")
(set-face-attribute 'org-level-4 nil :inherit 'variable-pitch :foreground "#b58900" :height 1.1 :family "Courier New")
(set-face-attribute 'org-level-5 nil :inherit 'variable-pitch :foreground "#2aa198" :height 1.0 :family "Courier New")
(set-face-attribute 'org-level-6 nil :inherit 'variable-pitch :foreground "pale green" :height 1.0 :family "Courier New")
(set-face-attribute 'org-level-8 nil :inherit 'variable-pitch :foreground "#268bd2" :family "Courier New")
(set-face-attribute 'org-todo nil :foreground "chocolate1" :weight 'bold)
(set-face-attribute 'sldb-condition-face nil :inherit font-lock-warning-face :foreground "gray81")
(set-face-attribute 'slime-repl-output-face nil :foreground "gray88")
(set-face-attribute 'variable-pitch nil :foreground "gray88" :family "Inconsolata" :height 1.0)

(set-frame-font "Inconsolata 12")
(global-font-lock-mode t)

(load-theme 'grandshell t)

; for now i want fringe mode for trying out diff-hl mode
; (set-fringe-mode '(0 . 0))

(setq display-time-mode 1)

(setq-default mode-line-format
      (list
       mode-line-mule-info
       mode-line-modified
       mode-line-frame-identification
       "    "
       '(:eval (substring
                (system-name) 0 (string-match "\\..+" (system-name))))
       ":"
       '(:eval (abbreviate-file-name default-directory))
       ;; the buffer name; the file name as a tool tip
       '(:eval (propertize "%b " 'face 'font-lock-type-face
                           'help-echo (buffer-file-name)))

       ;; line and column
       "(" ;; '%02' to set to 2 chars at least; prevents flickering
       (propertize "%02l" 'face 'font-lock-type-face) ","
       (propertize "%02c" 'face 'font-lock-type-face) 
       ") "

       ;; relative position, size of file
       "["
       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
       "/"
       (propertize "%I" 'face 'font-lock-constant-face) ;; size
       "] "

       ;; the current major mode for the buffer.
       "["

       '(:eval (propertize "%m" 'face 'font-lock-string-face
                           'help-echo buffer-file-coding-system))
       "] "


       "[" ;; insert vs overwrite mode, input-method in a tooltip
       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                           'face 'font-lock-preprocessor-face
                           'help-echo (concat "Buffer is in "
                                              (if overwrite-mode "overwrite" "insert") " mode")))

       ;; was this buffer modified since the last save?
       '(:eval (when (buffer-modified-p)
                 (concat ","  (propertize "Mod"
                                          'face 'font-lock-warning-face
                                          'help-echo "Buffer has been modified"))))

       ;; is this buffer read-only?
       '(:eval (when buffer-read-only
                 (concat ","  (propertize "RO"
                                          'face 'font-lock-type-face
                                          'help-echo "Buffer is read-only"))))  
       "] "

       ;; add the time, with the date and the emacs uptime in the tooltip
       '(:eval (propertize (format-time-string "%H:%M")
                           'help-echo
                           (concat (format-time-string "%c; ")
                                   (emacs-uptime "Uptime:%hh"))))
       " --"
       ;; i don't want to see minor-modes; but if you want, uncomment this:
       ;; minor-mode-alist  ;; list of minor modes
       "%-" ;; fill with '-'
       ))

; --------------------------------------------------


(setq visible-bell 0)
(setq ring-bell-function 'ignore)

(setq scroll-step            1
      scroll-conservatively  10000)

; hide backup files in a special directory
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq split-height-threshold 50)

(set-default 'tab-always-indent 'complete)

(setq typescript-indent-level 2)

(setq geiser-racket-binary "/Applications/Racket v6.10.1/bin/racket")
(setq python-shell-interpreter "python3")
(setq tcl-application "/usr/local/bin/tclsh8.6")


(provide 'mpr-init)
;;; mpr-init.el ends here
