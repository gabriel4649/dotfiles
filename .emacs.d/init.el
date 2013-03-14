;; set-up org babel
(setq org-babel-load-languages '((emacs-lisp . t)))
(setq org-confirm-babel-evaluate nil)
(require 'org-install)
(require 'org)
;; load neatly organized org file!
(org-babel-load-file "~/.emacs.d/Emacs.org")
