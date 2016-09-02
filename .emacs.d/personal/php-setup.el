;; Take care of extra dependencies
(prelude-ensure-module-deps '(php-mode geben ac-php company-php))

(add-hook 'php-mode-hook
          '(lambda ()
             (require 'company-php)
             (company-mode t)
             (add-to-list 'company-backends 'company-ac-php-backend )))

(autoload 'geben "geben" "PHP Debugger on Emacs" t)

(provide 'php-setup)
