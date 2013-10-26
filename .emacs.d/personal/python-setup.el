;; Take care of extra dependencies
(prelude-ensure-module-deps '(virtualenvwrapper jedi))

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.virtualenvs")

; Setup Jedi
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:setup)

(provide 'python-setup)
