;; Take care of extra dependencies
(prelude-ensure-module-deps '(solarized-theme sublimity))

;  Remove scrollbar
; http://emacs-fu.blogspot.com/2009/12/scrolling.html
(set-scroll-bar-mode nil)

;; Use Solarized light as the default theme
;; (load-theme 'solarized-light t)
;; Use Solarized dark as the default theme
(load-theme 'solarized-dark t)

(require 'sublimity)
(require 'sublimity-scroll)
;; (require 'sublimity-map)
;; (require 'sublimity-attractive)
(sublimity-mode 1)

(provide 'appearance)
