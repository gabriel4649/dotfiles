(require 'mu4e)
;; default
;; (setq mu4e-maildir "~/Maildir")

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/[Gmail].Sent Mail"   . ?s)
         ("/[Gmail].Trash"       . ?t)
         ("/[Gmail].All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; http://www.djcbsoftware.nl/code/mu/mu4e/Displaying-rich_002dtext-messages.html
(setq mu4e-html2text-command "html2text -utf8 -width 72")

;; Try to display images in mu4e
(setq
 mu4e-view-show-images t
 mu4e-view-image-max-width 800)

;; something about ourselves
(setq
 user-mail-address "gabriel.perez@gatech.edu"
 user-full-name  "Gabriel J. Pérez Irizarry")

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587

      ;; if you need offline mode, set these -- and create the queue dir
      ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
      smtpmail-queue-mail  nil
      smtpmail-queue-dir  "~/Maildir/queue/cur")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(setq
 mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
 mu4e-update-interval 300)             ;; update every 5 minutes

;; Mu4e key binding
(global-set-key (kbd "C-x M") 'mu4e)

(provide 'setup-mu4e)
