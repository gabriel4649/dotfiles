;; Take care of extra dependencies
(prelude-ensure-module-deps '(org org-bullets org-octopress
                                  audio-notes-mode calfw org-present))

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
          "* TODO %^{Brief Description} %^g\n\nAdded: %U" )

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

         ("s"
          "Shopping"
          checkitem
	  (file+headline "~/Ubuntu One/org/libreta.org" "Shopping")
          "[ ] %^{Item} \nAdded: %U" )

         ("r"
          "Reunion"
          entry
	  (file+headline "~/Ubuntu One/org/migtd.org" "Entrando")
          "* TODO Reunirme con %^{Persona(s) o grupo}  %^G\nSCHEDULED:%^{Date}t\nAdded: %U" )

         ("e"
          "Evento"
          entry
	  (file+headline "~/Ubuntu One/org/migtd.org" "Eventos")
          "* %^{Title}  %^G\nSCHEDULED:%^{Date}t\nAdded: %U" )

         ("a"
          "Anti-TODO"
          entry
	  (file+headline "~/Ubuntu One/org/migtd.org" "Anti-TODO")
          "* DONE %^{Title} \nAdded: %U" )
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
("@reunion" . ?r)
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

; Use IDO for org-mode
(setq org-completion-use-ido t)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; PDFs visited in Org-mode are opened in Evince (and not in the default choice) http://stackoverflow.com/a/8836108/789593
(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

;; Fork the work (async) of pushing to mobile
;; https://gist.github.com/3111823 ASYNC org mobile push...
;; http://kenmankoff.com/2012/08/17/emacs-org-mode-and-mobileorg-auto-sync
(require 'gnus-async)
;; Define a timer variable
(defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")
;; Push to mobile when the idle timer runs out
(defun org-mobile-push-with-delay (secs)
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'org-mobile-push)))
;; After saving files, start an idle timer after which we are going to push
(add-hook 'after-save-hook
 (lambda ()
   (if (or (eq major-mode 'org-mode) (eq major-mode 'org-agenda-mode))
     (dolist (file (org-mobile-files-alist))
       (if (string= (expand-file-name (car file)) (buffer-file-name))
           (org-mobile-push-with-delay 10)))
     )))

;; Run after midnight each day (or each morning upon wakeup?).
(run-at-time "00:01" 86400 '(lambda () (org-mobile-push-with-delay 1)))
;; Run 1 minute after launch, and once a day after that.
(run-at-time "1 min" 86400 '(lambda () (org-mobile-push-with-delay 1)))

;; watch mobileorg.org for changes, and then call org-mobile-pull
;; http://stackoverflow.com/questions/3456782/emacs-lisp-how-to-monitor-changes-of-a-file-directory
(defun install-monitor (file secs)
  (run-with-timer
   0 secs
   (lambda (f p)
     (unless (< p (second (time-since (elt (file-attributes f) 5))))
       (org-mobile-pull)))
   file secs))
(defvar monitor-timer (install-monitor (concat org-mobile-directory "/mobileorg.org") 30)
  "Check if file changed every 30 s.")

;; audio-notes-mode stuff
(require 'audio-notes-mode)
(setq anm/notes-directory "~/Ubuntu One/notas-audio")
(setq anm/goto-file "~/Ubuntu One/org/migtd.org")
(setq anm/hook-into-org-pull t)

;; Octopress stuff
(require 'org-octopress)
(setq org-octopress-directory-top       "~/coding/personal-blog/source")
(setq org-octopress-directory-posts     "~/coding/personal-blog/source/_posts")
(setq org-octopress-directory-org-top   "~/coding/personal-blog/source")
(setq org-octopress-directory-org-posts "~/coding/personal-blog/source/blog")
(setq org-octopress-setup-file          "~/coding/personal-blog/setupfile.org")

;; We want to activate emacs-calfw
(require 'calfw-cal)
(require 'calfw-org)

(autoload 'org-present "org-present" nil t)

(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images)))

(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-remove-inline-images)))

(provide 'setup-org-mode)
