; Add extra emacs modules to load path
; http://stackoverflow.com/questions/221365/emacs-lisp-how-to-add-a-folder-and-all-its-first-level-sub-folders-to-the-load
(let ((base "~/.emacs.d/elisp"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

; Save sessions
(desktop-save-mode 1)

; Automatically revert buffers
(global-auto-revert-mode 1)

; Setup key for auto-fill-mode
; http://www.emacswiki.org/emacs/AutoFillMode    
(global-set-key (kbd "C-c q") 'auto-fill-mode)

; Setup themes
(require 'color-theme)
(color-theme-initialize)

; Setup tango tango theme
; http://blog.nozav.org/post/2010/07/12/Updated-tangotango-emacs-color-theme
(require 'color-theme-tangotango)
(color-theme-tangotango)

; Org-mode stuff

; Org-mode key maps
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

; Activate org protocol
; http://orgmode.org/worg/org-contrib/org-protocol.html
(server-start)
(require 'org-protocol)

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org")

; Set agenda files
(setq org-agenda-files (file-expand-wildcards "~/Dropbox/org/*.org"))

; Set file for capture mode
(setq org-default-notes-file "~/Dropbox/org/capture.org")

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")

;; Setup the mobile directory
(setq org-mobile-directory "~/Dropbox/MobileOrg")

; Capture key
(define-key global-map "\C-cc" 'org-capture)

; Capture templates
; http://orgmode.org/worg/org-contrib/org-protocol.html#sec-6-1-1
(setq org-capture-templates
      (quote
       (("w"
         "Default template"
         entry
         (file+headline "~/org/capture.org" "Notes")
         "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
         :empty-lines 1)
        ;; ... more templates here ...
        )))

; Set tags
(setq org-tag-alist 
'(("@apartamento" . ?a) 
("@carro" . ?v) 
("@universidad" . ?u) 
("@pueblo de mayagüez" . ?p) 
("@montehiedra" . ?m) 
("computadora" . ?c) 
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

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

; Activate workgroups
; https://github.com/tlh/workgroups.el
(require 'workgroups)
(workgroups-mode 1)
(wg-load "~/.emacs.d/workgroups")

; Activate IDO
(require 'ido)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ido-mode (quote both) nil (ido)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
