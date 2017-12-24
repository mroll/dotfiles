(use-package nnir
  :init
  (setq gnus-select-method '(nntp "news.gmane.org"))
  (setq gnus-secondary-select-methods '((nnimap "matt"
                                                (nnimap-address "127.0.0.1")
                                                (nnimap-server-port 1143)
                                                (nnimap-stream starttls)
                                                (nnir-search-engine imap))
                                        (nnimap "mproll"
                                                (nnimap-address "127.0.0.1")
                                                (nnimap-server-port 1143)
                                                (nnimap-stream starttls)
                                                (nnir-search-engine imap))))
  (setq-default epa-file-cache-passphrase-for-symmetric-entryption t)
  (setq-default gnus-thread-sort-function
                '(gnus-thread-sort-by-most-recent-date
                  (not gnus-thread-sort-by-number)))
  (setq gnus-use-cache t)
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  (setq gnus-thread-hide-subtree t)
  (setq gnus-thread-ignore-subject t)
  ;; (setq user-full-name "Matt Roll"
  ;;       user-mail-address "mproll@protonmail.com")
  ;; Setup to send email through SMTP
  ;; (setq message-send-mail-function 'smtpmail-send-it
  ;;       smtpmail-default-smtp-server "smtp-protonmail:matt"
  ;;       smtpmail-smtp-service 1025
  ;;       smtpmail-local-domain "fool")
  ;; http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
  (setq gnus-use-correct-string-widths nil))

(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-topic-topology '(("Gnus" visible)
                                 (("matt@matthewroll.com" visible))
                                 (("mproll@protonmail.com" visible))))

     (setq gnus-topic-alist '(("matt@matthewroll.com" ; the key of topic
                               "nnimap+matt:INBOX"
                               "nnimap+matt:Sent"
                               "nnimap+matt:Spam"
                               "nnimap+matt:Trash")
                              ("mproll@protonmail.com" ; the key of topic
                               "nnimap+mproll:INBOX"
                               "nnimap+mproll:Sent"
                               "nnimap+mproll:Spam"
                               "nnimap+mproll:Trash")))))
