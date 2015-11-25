(setq wl-thread-indent-level 2)
(setq wl-thread-have-younger-brother-str "+"
      wl-thread-youngest-child-str "+"
      wl-thread-vertical-str "|"
      wl-thread-horizontal-str "-"
      wl-thread-space-str " ")

;; ignore  all fields
(setq wl-message-ignored-field-list '("^.*:"))

;; ..but these five
(setq wl-message-visible-field-list
'("^To:"
  "^Cc:"
  "^From:"
  "^Subject:"
  "^Date:"))
    
(setq wl-summary-width nil)
(setq wl-summary-indent-length-limit nil)
(setq wl-summary-from-width nil)
(setq wl-summary-always-sticky-folder-list t)

;; IMAP, gmail:
(setq elmo-imap4-default-server "mail.messagingengine.com"
      elmo-imap4-default-user "mkastner@fastmail.fm"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl
      )

;; SMTP
(setq wl-smtp-connection-type 'starttls
      wl-smtp-posting-port 587
      wl-smtp-authenticate-type "plain"
      wl-smtp-posting-user "mkastner@fastmail.fm"
      wl-smtp-posting-server "mail.messagingengine.com"
      wl-local-domain "fastmail.fm"
      wl-message-id-domain "mail.messagingengine.com")

(setq wl-from "Marc A. Kastner <mkastner@fastmail.fm>"

      ;;all system folders (draft, trash, spam, etc) are placed in the
      ;;[Gmail]-folder, except inbox. "%" means it's an IMAP-folder
      wl-default-folder "%INBOX"
      wl-draft-folder   "%INBOX.Drafts"
      wl-trash-folder   "%INBOX.Trash"
      wl-fcc            "%INBOX.Sent Items"

      ;; mark sent messages as read (sent messages get sent back to you and
      ;; placed in the folder specified by wl-fcc)
      wl-fcc-force-as-read    t

      ;;for when auto-compleating foldernames
      wl-default-spec "%")

(setq wl-folder-hierarchy-access-folders
      '("^.\\([^/.]+[/.]\\)*[^/.]+\\(:\\|@\\|$\\)"
        "^-[^.]*\\(:\\|@\\|$\\)"
        "^@$"
        "^'$"))

(setq mime-w3m-safe-url-regexp nil)
(setq mime-w3m-display-inline-images t)
