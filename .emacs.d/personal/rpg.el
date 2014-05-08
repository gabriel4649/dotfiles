(add-to-list 'load-path "elisp/habitrpg/")

(require 'habitrpg)

(add-hook 'org-after-todo-state-change-hook 'habitrpg-add 'append)
;;(add-hook 'org-capture-before-finalize-hook'habitrpg-add 'append)
;; For adding tasks from org mode
(global-set-key (kbd "C-c C-x h") 'habitrpg-add)
;; Status buffer - use C-h m to see the keybindings
;; C-c C-c - upvote task or buy reward
;; C-c C-d - downvote task
;; t - bring up manage menu, which adds or deletes tasks
(global-set-key (kbd "<f9> a") 'habitrpg-status)
;; Continuously update a habit attache to a clocking task
;; (add-hook 'org-clock-in-hook 'habitrpg-clock-in)
;; (add-hook 'org-clock-out-hook 'habitrpg-clock-out)
;; List of habits to check for when clocking a task
;; (add-to-list 'hrpg-tags-list "PROGRAMMING")
;; (add-to-list 'hrpg-tags-list "WORK")
(setq habitrpg-api-user "c873af22-f475-4df0-8d77-e181086b75f8")
(setq habitrpg-api-token "f4186108-ba57-4671-b66b-b79c36c61db4")
