;; Take care of extra dependencies
(prelude-ensure-module-deps '(solarized-theme nyan-mode sublimity))

;; Use Solarized dark as the default theme
(load-theme 'solarized-dark t)

;; Adds nyan scroll
(require 'nyan-mode)
(nyan-mode)

(require 'sublimity)
(require 'sublimity-scroll) ;; smooth scrolling
(require 'sublimity-attractive) ;; distraction free mode
(sublimity-attractive-hide-bars) ;; hide scroll bars
;; (require 'sublimity-map)
(sublimity-mode 1)

(provide 'appearance)
