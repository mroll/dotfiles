;;; gnus.el -- newsreader and mail configuration
;;

;;; Commentary:
;;

;;; Code:
;;


(defvar mml2015-encrypt-to-self t)
(defvar epa-file-cache-passphrase-for-symmetric-entryption t)
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)


(setq gnus-agent nil)
(setq gnus-message-archive-group nil)
(setq mail-host-address "fool")
;; (setq gnus-use-cache nil)
(setq gnus-select-method '(nntp "news.gmane.org"))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                      ;; press 'E' to expire email
                      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                      (nnmail-expiry-wait 90)))

(defvar gnus-thread-sort-function
  '(gnus-thread-sort-by-most-recent-date
    (not gnus-thread-sort-by-number)))
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

;; press "o" to view all groups
(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

(define-key gnus-group-mode-map
  ;; list all the subscribed groups even they contain zero un-read messages
  (kbd "o") 'my-gnus-group-list-subscribed-groups)

;; ;; Setup to send email through SMTP
;; (setq message-send-mail-function 'message-send-mail-with-sendmail)
;; (defvar sendmail-program "/usr/local/bin/msmtp")

; Send email via Gmail:
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com")

;; This is needed to allow msmtp to do its magic:
; (setq message-sendmail-f-is-evil t)
;;need to tell msmtp which account we're using
; (setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq gnus-permanently-visible-groups ".*")
(setq gnus-use-correct-string-widths nil)
;; (setq gnus-parameters
;;       '(("mproll:INBOX"
;;         (display . all)
;;         (posting-style
;;           (name "matt roll")
;;           (address "matt@idiomatic.io"))
;;         (expiry-target . delete))))

;; Use topics per default
;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;; (eval-after-load 'gnus-topic
;;   '(progn

;;      (setq-default gnus-topic-topology '(("Gnus" visible)
;;                                          (("mproll" visible))))

;;     (setq-default gnus-topic-alist '(("mproll" ; the key of topic
;;                                       "nnimap+mproll:INBOX"
;;                                       "nnimap+mproll:Sent"
;;                                       "nnimap+mproll:Spam"
;;                                       "nnimap+mproll:Trash")))))

(set-face-attribute 'variable-pitch nil :family "Inconsolata")
