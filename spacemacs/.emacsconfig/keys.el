(define-key evil-insert-state-map (kbd "C-w") 'backward-kill-word)

;; keybindings for controlling frame location
(global-set-key (kbd "M-<up>")           (lambda () (interactive) (move-frame-up 3)))
(global-set-key (kbd "M-<down>")         (lambda () (interactive) (move-frame-down 3)))
(global-set-key (kbd "M-<left>")         (lambda () (interactive) (move-frame-left 5)))
(global-set-key (kbd "M-<right>")        (lambda () (interactive) (move-frame-right 5)))

;; keybindings for controlling frame dimensions
(global-set-key (kbd "C-M-<down>")         (lambda () (interactive) (enlarge-frame 5)))
(global-set-key (kbd "C-M-<right>")        (lambda () (interactive) (enlarge-frame-horizontally 5)))
(global-set-key (kbd "C-M-<up>")           (lambda () (interactive) (shrink-frame 5)))
(global-set-key (kbd "C-M-<left>")         (lambda () (interactive) (shrink-frame-horizontally 5)))
