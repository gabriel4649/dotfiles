;; Take care of extra dependencies
(prelude-ensure-module-deps '(auto-complete multiple-cursors
                                       zencoding-mode minimap jedi))

;;; Add extra emacs modules to load path
;;;; http://stackoverflow.com/questions/221365/emacs-lisp-how-to-add-a-folder-and-all-its-first-level-sub-folders-to-the-load
(let ((base "~/.emacs.d/personal/elisp"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; Add key-binding for auto-fill-mode
;; http://emacswiki.org/emacs/AutoFillMode
(global-set-key (kbd "C-c q") 'auto-fill-mode)
; Turn on auto-fill automatically for org-mode files
(add-hook 'org-mode-hook 'turn-on-auto-fill)

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

;; Arduino stuff
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

(provide 'personal-init)
