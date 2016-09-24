function emacs
	emacsclient -t $argv
end 

set EDITOR emacsclient -t

# virtualfish for virtualenvs
# https://github.com/adambrenecki/virtualfish
eval (python -m virtualfish)
