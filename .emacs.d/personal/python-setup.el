;; Take care of extra dependencies
(prelude-ensure-module-deps '(virtualenvwrapper))

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.virtualenvs")

(provide 'python-setup)