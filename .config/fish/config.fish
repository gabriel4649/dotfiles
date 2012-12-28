set PATH ~/frameworks/play $PATH

function resume
     ssh climatewarrior_gabrieljperez@ssh.phx.nearlyfreespeech.net 
end

function cuda
     ssh climatewarrior@136.145.116.227	
end

function emacs
	emacsclient -t $argv
end 

# Fix for fish in emacs
# https://github.com/fish-shell/fish-shell/issues/107
function fish_title
  true
end
