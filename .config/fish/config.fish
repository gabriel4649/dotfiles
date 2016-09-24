function emacs
	emacsclient -t $argv
end

function emacs-restart
         killall emacs
         /usr/bin/emacs --daemon
end 

set EDITOR emacsclient -t

# virtualfish for virtualenvs
# https://github.com/adambrenecki/virtualfish
eval (python -m virtualfish)
