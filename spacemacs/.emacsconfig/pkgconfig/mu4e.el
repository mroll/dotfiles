(setq mail-user-agent 'mu4e-user-agent)

;; default
(setq mu4e-maildir "~/.mail/pm")
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-trash-folder  "/Trash")
(setq mu4e-refile-folder  "/All")

(setq mu4e-headers-fields
      '( (:human-date       .   12)
         (:flags            .    6)
         (:mailing-list     .   10)
         (:from             .   22)
         (:thread-subject   .   nil)))

(setq mu4e-headers-full-search nil)
(setq mu4e-headers-result-limit 1000)
(setq message-kill-buffer-on-exit t)

(setq mu4e-view-show-images t)

(mu4e-alert-set-default-style 'notifier)
(mu4e-alert-enable-mode-line-display)

;; This allows me to use 'helm' to select mailboxes
(setq mu4e-completing-read-function 'completing-read)
;; Why would I want to leave my message open after I've sent it?
(setq message-kill-buffer-on-exit t)
;; Don't ask for a 'context' upon opening mu4e
(setq mu4e-context-policy 'pick-first)
;; Don't ask to quit... why is this the default?
(setq mu4e-confirm-quit nil)

(setq user-mail-address "mproll@pm.me")
(setq smtpmail-default-smtp-server "127.0.0.1"
      smtpmail-smtp-server "127.0.0.1"
      smtpmail-smtp-service 1025)
(setq message-send-mail-function 'smtpmail-send-it)

(setq mu4e-update-interval 300)
(setq mu4e-view-show-addresses 't)

(setq mu4e-get-mail-command "offlineimap")

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"                       . ?i)
         ("/Sent"                        . ?s)
         ("/Archive"                     . ?a)
         ("/Trash"                       . ?T)
         ("/Folders.travel"              . ?t)
         ("/Folders.finance"             . ?f)
         ("/Folders.receipts"            . ?r)
         ("/Folders.important.contacts"  . ?c)
         ("/Folders.subscriptions"       . ?u)
         ("/Folders.projects"            . ?p)))

(bbdb-initialize 'gnus
                 ;;bbdb-insinuate-gnus adds bindings for the default
                 ;;keys to Gnus and configures Gnus to notify the
                 ;;BBDB when new messages are loaded. This
                 ;;notification is required if the BBDB is to be
                 ;;able to display BBDB entries for messages
                 ;;displayed in Gnus.

                 'message
                 ;; bbdb-insinuate-message adds a binding for M-TAB
                 ;; to Message mode. This will enable completion of
                 ;; addressees based on BBDB records.

                 'mu4e
                 'anniv)

(bbdb-mua-auto-update-init 'gnus 'message 'mu4e)

;; size of the bbdb popup
(setq bbdb-mua-pop-up-window-size 1)

;; how to handle new entries
;; (setq bbdb-mua-update-interactive-p '(query . create))

;; look at every address; not only the first one
;; (setq bbdb-message-all-addresses t)

(setq bbdb-phone-style 'nil
      bbdb-add-mails t
      bbdb-completion-list t
      )
