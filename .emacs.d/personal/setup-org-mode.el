;; Set to the location of your Org files on your local system
(setq org-directory "~/Ubuntu One/org")

; Set agenda files
(setq org-agenda-files (file-expand-wildcards "~/Ubuntu One/org/*.org"))

; Set file for capture mode
(setq org-default-notes-file "~/Ubuntu One/org/capture.org")

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Ubuntu One/org/flagged.org")

;; Setup the mobile directory
(setq org-mobile-directory "~/Ubuntu One/MobileOrg")

; Capture key
(define-key global-map "\C-cc" 'org-capture)

; Org-mode key maps
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

; Activate org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; Activate org protocol
; http://orgmode.org/worg/org-contrib/org-protocol.html
(require 'org-protocol)

; Capture templates
; http://orgmode.org/worg/org-contrib/org-protocol.html#sec-6-1-1
(setq org-capture-templates
      (quote
       (("i"
         "Internet"
         entry
         (file+headline "~/Ubuntu One/org/capture.org" "Notes")
         "* %^{Title} %u, %c\n\n  %i"
         :empty-lines 1)

         ("t"
          "TODO"
	  entry
	  (file+headline "~/Ubuntu One/org/migtd.org" "Entrando")
          "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" )

         ("w"
          "WAITING"
	  entry
	  (file+headline "~/Ubuntu One/org/migtd.org" "Esperando")
          "* WAITING %^{Brief Description} %^g\n%?\nAdded: %U" )

         ("d"
          "diario"
	  entry
	  (file+headline "~/Ubuntu One/org/diario.org" "Entradas")
          "* %^{Title} \nAdded: %U" )
        ;; ... more templates here ...

        )))

; Set tags
(setq org-tag-alist
'(("@apartamento" . ?a)
("@carro" . ?v)
("@universidad" . ?u)
("@downtown" . ?d)
("@san juan" . ?s)
("@casa" . ?m)
("@pensar" . ?p)
("computadora" . ?c)
("iPad" . ?i)
("email" . ?e)
("telefono" . ?t)))


; Set to-do keywords
(setq org-todo-keywords
       '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s)" "|" "DONE(d!)" "CANCELED(c@)")))

;Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Stop using paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; PDFs visited in Org-mode are opened in Evince (and not in the default choice) http://stackoverflow.com/a/8836108/789593
(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

  ; Auto org-mobile push https://gist.github.com/mrvdb/3111823/download#
  ;; Show a notification when a push has been completed
  ;; (require 'notifications)
  ;; (defun notify-push (result)
  ;;   (notifications-notify
  ;;    :title "Push complete"
  ;;    :body  (format "Org-mobile-push: %s" result)
  ;;   )
  ;; )

  ;; ;; Fork the work of pushing to mobile
  ;; (require 'async)
  ;; (defun fork-org-push-mobile ()
  ;;   (async-start
  ;;    ;; What to do in the child process
  ;;    `(lambda ()
  ;;       ,(async-inject-variables "org-\\(mobile-\\|directory\\)")
  ;;       (org-mobile-push))

  ;;    ; What to do when it finishes
  ;;    (lambda (result)
  ;;      (notify-push result))))

  ;; ;; Define a timer variable
  ;; (defvar org-mobile-push-timer nil
  ;;   "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

  ;; ;; Push to mobile when the idle timer runs out
  ;; (defun org-mobile-push-with-delay (secs)
  ;;   (when org-mobile-push-timer
  ;;     (cancel-timer org-mobile-push-timer))
  ;;   (setq org-mobile-push-timer
  ;;         (run-with-idle-timer
  ;;          (* 1 secs) nil 'fork-org-push-mobile)))

  ;; After saving files, start a 30 seconds idle timer after which we
  ;; are going to push
  ;; (add-hook 'after-save-hook
  ;; (lambda ()
  ;; (when (eq major-mode 'org-mode)
  ;; (dolist (file (org-mobile-files-alist))
  ;; (if (string= (expand-file-name (car file)) (buffer-file-name))
  ;; (org-mobile-push-with-delay 30)))
  ;; )))

  ;; At least run it once a day, but no need for a delay this time
  ;; (run-at-time "00:05" 86400 '(lambda () (org-mobile-push-with-delay 1)))

(provide 'setup-org-mode)
