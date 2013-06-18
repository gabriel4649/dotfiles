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

;; Run ispell on a word
(global-set-key (kbd "<f7>") 'ispell-word)

(provide 'ispell-enhancements)
