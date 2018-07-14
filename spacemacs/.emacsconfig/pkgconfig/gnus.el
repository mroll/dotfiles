;;; gnus.el -- newsreader and mail configuration
;;

;;; Commentary:
;;

;;; Code:
;;


;; (defvar mml2015-encrypt-to-self t)
;; (defvar epa-file-cache-passphrase-for-symmetric-entryption t)
;; (add-hook 'message-setup-hook 'mml-secure-message-encrypt)


(setq gnus-agent nil)
(setq gnus-message-archive-group nil)
(setq mail-host-address "fitz")
;; (setq gnus-use-cache nil)
(setq gnus-select-method '(nntp "news.gmane.org"))
(setq gnus-secondary-select-methods '((nnimap "mproll"
                                              (nnimap-address "127.0.0.1")
                                              (nnimap-server-port 1143)
                                              (nnimap-stream starttls)
                                              (nnir-search-engine imap))
                                      (nntp "news.gwene.org")))
(defvar gnus-thread-sort-function
  '(gnus-thread-sort-by-most-recent-date
    (not gnus-thread-sort-by-number)))
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)
;; Setup to send email through SMTP
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(defvar sendmail-program "/usr/local/bin/msmtp")
;; This is needed to allow msmtp to do its magic:
(setq message-sendmail-f-is-evil t)
;;need to tell msmtp which account we're using
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq gnus-permanently-visible-groups ".*")
(setq gnus-use-correct-string-widths nil)
(setq gnus-parameters
      '(("mproll:INBOX"
        (display . all)
        (posting-style
          (name "matt roll")
          (address "mproll@pm.me"))
        (expiry-target . delete))))

;; Use topics per default
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(eval-after-load 'gnus-topic
  '(progn

     (setq-default gnus-topic-topology '(("Gnus" visible)
                                         (("mproll" visible))))

    (setq-default gnus-topic-alist '(("mproll" ; the key of topic
                                      "nnimap+mproll:INBOX"
                                      "nnimap+mproll:Sent"
                                      "nnimap+mproll:Spam"
                                      "nnimap+mproll:Trash")))))

(set-face-attribute 'variable-pitch nil :family "Inconsolata")
