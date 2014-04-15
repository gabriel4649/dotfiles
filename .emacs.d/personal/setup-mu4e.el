(require 'mu4e)
(require 'org-mu4e)

;; Run IMAP filter
(add-hook 'mu4e-update-pre-hook 'etc/imapfilter)
(defun etc/imapfilter ()
  (message "Running imapfilter...")
  (with-current-buffer (get-buffer-create " *imapfilter*")
    (goto-char (point-max))
    (insert "---\n")
    (call-process "imapfilter" nil (current-buffer) nil "-v"))
  (message "Running imapfilter...done"))

;; default
(setq mu4e-maildir "~/Maildir")

(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-trash-folder  "/Trash")

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)
        ("/Sent" . ?s)
        ("/Trash" . ?t)
        ("/Archives.2014" . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; http://www.djcbsoftware.nl/code/mu/mu4e/Displaying-rich_002dtext-messages.html
(setq mu4e-html2text-command "html2text -utf8 -width 72")

;; Try to display images in mu4e
(setq
 mu4e-view-show-images t
 mu4e-view-image-max-width 800)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; something about ourselves
(setq
 user-mail-address "gabriel.perez@gatech.edu"
 user-full-name  "Gabriel J. Pérez Irizarry"
 message-signature
 (concat
  "If possible please communicate with me using PGP encryption to avoid snooping"
  " by third parties. PGP Key ID: 6F769FFD"))

;; When replying to an email I want to use the address I received this message to as the sender of the reply.
(add-hook 'mu4e-compose-pre-hook
          (defun my-set-from-address ()
            "Set the From address based on the To address of the original."
            (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
              (if msg
                  (setq user-mail-address
                        (cond
                         ((mu4e-message-contact-field-matches msg :to "gabrieljoel@gmail.com")
                          "gabrieljoel@gmail.com")
                         ((mu4e-message-contact-field-matches msg :to "gabriel.perez5@upr.edu")
                          "gabriel.perez5@upr.edu")
                         ((mu4e-message-contact-field-matches msg :to "gabriel.perez@gatech.edu")
                          "gabriel.perez@gatech.edu")
                         ((mu4e-message-contact-field-matches msg :to "freeculture.rum@gmail.com")
                          "freeculture.rum@gmail.com")
                         (t "gabriel.perez@gatech.edu")))))))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'ssl
      smtpmail-default-smtp-server "neomailbox.net"
      smtpmail-smtp-server "neomailbox.net"
      smtpmail-smtp-service 465
      smtpmail-auth-credentials (expand-file-name "~/.authinfo")

      ;; if you need offline mode, set these -- and create the queue dir
      ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
      smtpmail-queue-mail  nil
      smtpmail-queue-dir  "~/Maildir/queue/cur")


(setq message-kill-buffer-on-exit t ;; don't keep message buffers around
      mu4e-compose-dont-reply-to-self t ;; don't reply to myself
      mu4e-use-fancy-chars t ; use fancy characters
      )


; Wrap lines
(add-hook 'mu4e-view-mode-hook
          'longlines-mode)

(setq
 mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
 mu4e-update-interval 300)             ;; update every 5 minutes

;; Mu4e key binding
(global-set-key (kbd "C-x n") 'mu4e)

;; message view action
;; http://www.brool.com/index.php/using-mu4e
(defun mu4e-msgv-action-view-in-browser (msg)
  "View the body of the message in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
       "<html>"
       "<head><meta http-equiv=\"content-type\""
       "content=\"text/html;charset=UTF-8\">"
       html))
    (browse-url (concat "file://" tmpfile))))
(add-to-list 'mu4e-view-actions
             '("View in browser" . mu4e-msgv-action-view-in-browser) t)

(provide 'setup-mu4e)
