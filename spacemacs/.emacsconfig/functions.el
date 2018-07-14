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

(defun insert-multiline-brace ()
  (interactive)
  (insert " {
}")
  (left-char)
  (indent-for-tab-command)
  (evil-open-above 1))
(define-key evil-insert-state-map (kbd "C-j") 'insert-multiline-brace)

(defun md-footnote (n content)
  (interactive "snumber: \nscontent: \n")
  (save-excursion
    (insert (format "<sup>[%s](#fn%s)</sup>" n n))
    (goto-char (point-max))
    (insert (format "\n<a name=\"fn%s\">%s</a>: %s" n n content))))

(defun mpr/erc-connect-freenode ()
  (erc :server "matthewroll.com" :port 5000 :nick "le4fy" :password mpr/znc-pass-freenode))

(defun mpr/erc-connect-snoonet ()
  (erc :server "matthewroll.com" :port 5000 :nick "le4fy" :password mpr/znc-pass-snoonet))
;; (mpr/erc-connect-freenode)
;; (mpr/erc-connect-snoonet)

(defun split-arglist-onto-separate-lines ()
  (interactive)
  (save-excursion
    (let ((parens '()))
      (backward-up-list)
      (forward-char 1)
      (newline)
      (indent-for-tab-command)
      (while (not (and (null parens)
                       (char-equal (char-after) ?\))))
        (cond ((char-equal (char-after) ?\()
               (push ?\( parens))
              ((char-equal (char-after) ?\))
               (pop parens))
              ((char-equal (char-after) ?,)
               (progn (forward-char)
                      (newline)
                      (indent-for-tab-command))))
        (forward-char))
      (newline)
      (indent-for-tab-command))))
