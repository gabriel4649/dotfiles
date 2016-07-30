set PATH ~/.scripts/ $PATH

function emacs
	emacsclient -t $argv
end 

set EDITOR emacsclient -t

# virtualfish for virtualenvs
# https://github.com/adambrenecki/virtualfish
eval (python -m virtualfish)

# Fix for fish in emacs
# https://github.com/fish-shell/fish-shell/issues/107
function fish_title
  true
end

alias sf "symfony"
