;; Take care of extra dependencies
(prelude-ensure-module-deps '(slime))

;; Slime js
(add-to-list 'load-path "~/.npm/swank-js/0.0.5/package/")
(slime-setup '(slime-js slime-repl))
(setq slime-js-swank-command "/usr/local/bin/swank-js")
(setq slime-js-swank-args '())

(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))

;; For slime js
(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
            (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))

(provide 'setup-slimejs)
