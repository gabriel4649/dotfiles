;; Take care of extra dependencies
(prelude-ensure-module-deps '(auto-complete multiple-cursors))

; Activate Multiple Cursors
; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
; Setup keybindings for multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(provide 'personal-init)
