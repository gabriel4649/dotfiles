set PATH ~/.scripts/ $PATH

set PATH ~/frameworks/ $PATH

function resume
     ssh climatewarrior_gabrieljperez@ssh.phx.nearlyfreespeech.net 
end

function curioso_blog
     ssh climatewarrior_curioso@ssh.phx.nearlyfreespeech.net 
end

function curioso_shop
     ssh climatewarrior_curioso-shop@ssh.phx.nearlyfreespeech.net 
end

function cuda
     ssh climatewarrior@136.145.116.227	
end

function emacs
	emacsclient -t $argv
end 

set EDITOR emacsclient -t

# Source virtualfish for virtualenvs
# https://github.com/adambrenecki/virtualfish
set VIRTUALFISH_COMPAT_ALIASES 1
. ~/.scripts/virtualfish/virtual.fish

# Fix for fish in emacs
# https://github.com/fish-shell/fish-shell/issues/107
function fish_title
  true
end
