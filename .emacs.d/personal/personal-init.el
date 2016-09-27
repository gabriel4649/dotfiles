;; Take care of extra dependencies
(prelude-ensure-module-deps '(multiple-cursors neotree all-the-icons))

; Activate Multiple Cursors
; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
; Setup keybindings for multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Pretty icons for neo tree
;; https://github.com/domtronn/all-the-icons.el
(require 'all-the-icons)

;; Set neotree theme to the icons
(setq neo-theme 'icons)

; Project tree view
; https://www.emacswiki.org/emacs/NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

; Integrate with projectile
(setq projectile-switch-project-action 'neotree-projectile-action)

(provide 'personal-init)
