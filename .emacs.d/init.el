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

; Setup themes
(require 'color-theme)
(color-theme-initialize)

; Setup tango tango theme
; http://blog.nozav.org/post/2010/07/12/Updated-tangotango-emacs-color-theme
(require 'color-theme-tangotango)
(color-theme-tangotango)

; Org-mode stuff
(setq org-tag-alist 
'(("@apartamento" . ?a) 
("@universidad" . ?u) 
("@pueblo de mayagüez" . ?p) 
("@montehiedra" . ?m) 
("computadora" . ?c) 
("email" . ?e) 
("telefono" . ?t)))

(setq org-todo-keywords
       '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(setq org-agenda-files (file-expand-wildcards "~/Dropbox/org/*.org"))
(setq org-mobile-directory "~/Dropbox/MobileOrg")

'(org-refile-targets (quote (("migtd.org" :maxlevel . 2) 
                             ("algundia.org" :level . 2))))

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
