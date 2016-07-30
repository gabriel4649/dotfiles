;; Take care of extra dependencies
(prelude-ensure-module-deps '(solarized-theme sublimity))

;  Remove scrollbars
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; Use Solarized light as the default theme
;; (load-theme 'solarized-light t)
;; Use Solarized dark as the default theme
(load-theme 'solarized-dark t)

(require 'nyan-mode)
(nyan-mode)

(require 'sublimity)
(require 'sublimity-scroll)
;; (require 'sublimity-map)
(require 'sublimity-attractive)
(sublimity-mode 1)

(provide 'appearance)
