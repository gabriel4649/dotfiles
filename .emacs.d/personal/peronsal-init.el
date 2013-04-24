;;; init-file -- Just my init file
;;; Commentary: Add extra emacs modules to load path
;;;; http://stackoverflow.com/questions/221365/emacs-lisp-how-to-add-a-folder-and-all-its-first-level-sub-folders-to-the-load
(let ((base "~/.emacs.d/personal/elisp"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

; Remove scrollbar
; http://emacs-fu.blogspot.com/2009/12/scrolling.html
(set-scroll-bar-mode nil)

; Enable line number display
(global-linum-mode t)

;; use soothe as the default theme
(require 'soothe-theme)

;; Add key-binding for auto-fill-mode
;; http://emacswiki.org/emacs/AutoFillMode
(global-set-key (kbd "C-c q") 'auto-fill-mode)
; Turn on auto-fill automatically for org-mode files
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Change dictionaries
;; http://www.emacswiki.org/emacs/FlySpell
(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "english") "espanol" "english")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(global-set-key (kbd "<f8>")   'fd-switch-dictionary)

;; Copy and paste in ansi-term plus other imrpovements
;; http://echosa.github.io/blog/2012/06/06/improving-ansi-term/
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defvar my-term-shell "/usr/bin/fish")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste))

(add-hook 'term-mode-hook 'my-term-hook)

;; Global auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)

; Activate Multiple Cursors
; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
; Setup keybindings for multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

; Activate minimap
; http://www.emacswiki.org/emacs/MiniMap
(require 'minimap)

; Activate zencoding
; https://github.com/rooney/zencoding
(require 'zencoding-mode)
;(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

(autoload 'jedi:setup "jedi" nil t)
(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:setup)

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
