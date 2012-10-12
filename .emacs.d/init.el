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

; Activate IDO
(require 'ido)
(ido-mode t)

; Setup theme
(load-theme 'tango-dark t)

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

; Activate ELPA
(require 'package)
(package-initialize)

; Add Marmalade repo
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") 

; Activate Multiple Cursors
; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors) 
; Setup keybindings for multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

; Mercurial mode a la maggit
; https://github.com/ananthakumaran/monky
(require 'monky)
(setq monky-process-type 'cmdserver)

; Enable pomodoro.el
; https://github.com/docgnome/pomodoro.el
(require 'pomodoro)

(defvar my-desktop-session-dir
  (concat (getenv "HOME") "/.emacs.d/desktop-sessions/")
  "*Directory to save desktop sessions in")

(defvar my-desktop-session-name-hist nil
  "Desktop session name history")

; Desktop session management
; http://scottfrazersblog.blogspot.com/2009/12/emacs-named-desktop-sessions.html
(require 'desktop)

(defun my-desktop-save (&optional name)
  "Save desktop by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Save session" t)))
  (when name
    (make-directory (concat my-desktop-session-dir name) t)
    (desktop-save (concat my-desktop-session-dir name) t)))

(defun my-desktop-save-and-clear ()
  "Save and clear desktop."
  (interactive)
  (call-interactively 'my-desktop-save)
  (desktop-clear)
  (setq desktop-dirname nil))

(defun my-desktop-read (&optional name)
  "Read desktop by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Load session")))
  (when name
    (desktop-clear)
    (desktop-read (concat my-desktop-session-dir name))))

(defun my-desktop-change (&optional name)
  "Change desktops by name."
  (interactive)
  (let ((name (my-desktop-get-current-name)))
    (when name
      (my-desktop-save name))
    (call-interactively 'my-desktop-read)))

(defun my-desktop-name ()
  "Return the current desktop name."
  (interactive)
  (let ((name (my-desktop-get-current-name)))
    (if name
        (message (concat "Desktop name: " name))
      (message "No named desktop loaded"))))

(defun my-desktop-get-current-name ()
  "Get the current desktop name."
  (when desktop-dirname
    (let ((dirname (substring desktop-dirname 0 -1)))
      (when (string= (file-name-directory dirname) my-desktop-session-dir)
        (file-name-nondirectory dirname)))))

(defun my-desktop-get-session-name (prompt &optional use-default)
  "Get a session name."
  (let* ((default (and use-default (my-desktop-get-current-name)))
         (full-prompt (concat prompt (if default
                                         (concat " (default " default "): ")
                                       ": "))))
    (completing-read full-prompt (and (file-exists-p my-desktop-session-dir)
                                      (directory-files my-desktop-session-dir))
                     nil nil nil my-desktop-session-name-hist default)))

(defun my-desktop-kill-emacs-hook ()
  "Save desktop before killing emacs."
  (when (file-exists-p (concat my-desktop-session-dir "last-session"))
    (setq desktop-file-modtime
          (nth 5 (file-attributes (desktop-full-file-name (concat my-desktop-session-dir "last-session"))))))
  (my-desktop-save "last-session"))

(add-hook 'kill-emacs-hook 'my-desktop-kill-emacs-hook)

; Automatically revert buffers
(global-auto-revert-mode 1)

; Setup key for auto-fill-mode
; http://www.emacswiki.org/emacs/AutoFillMode    
(global-set-key (kbd "C-c q") 'auto-fill-mode)

; Sane copy and paste
; http://www.emacswiki.org/emacs/CopyAndPaste
(global-set-key [(shift delete)] 'clipboard-kill-region)
(global-set-key [(control insert)] 'clipboard-kill-ring-save)
(global-set-key [(shift insert)] 'clipboard-yank)

; Org-mode stuff

; Org-mode key maps
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

; Add hooks to automatically push and pull for mobile org
; http://stackoverflow.com/questions/8432108/how-to-automatically-do-org-mobile-push-org-mobile-pull-in-emacs
(add-hook 'after-init-hook 'org-mobile-pull)
(add-hook 'kill-emacs-hook 'org-mobile-push) 

; Activate org protocol
; http://orgmode.org/worg/org-contrib/org-protocol.html
(require 'org-protocol)

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

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

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

; Activate workgroups
; https://github.com/tlh/workgroups.el
(require 'workgroups)
(workgroups-mode 1)
(wg-load "~/.emacs.d/workgroups")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("21d9280256d9d3cf79cbcf62c3e7f3f243209e6251b215aede5026e0c5ad853f" default)))
 '(org-agenda-files (quote ("~/Ubuntu One/org/algundia.org" "~/Ubuntu One/org/capture.org" "~/Ubuntu One/org/diario.org" "~/Ubuntu One/org/flagged.org" "~/Ubuntu One/org/habitos.org" "~/Ubuntu One/org/libreta.org" "~/Ubuntu One/org/migtd.org" "~/Ubuntu One/org/recordatorios.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
