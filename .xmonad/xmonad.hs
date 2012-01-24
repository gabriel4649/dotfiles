import XMonad
import XMonad.Config.Gnome

-- Great intro: http://www.linuxandlife.com/2011/11/how-to-configure-xmonad-arch-linux.html

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-launcher" --> doIgnore
    , className =? "Gimp" --> doFloat  
    , className =? "Firefox" --> doShift "1:web"
    ])

myWorkspaces = ["1:web","2:emacs","3","4:docs","5","6","7","8","9"]  


main = xmonad gnomeConfig 
	{ manageHook = myManageHook 
         , modMask = mod4Mask
         , focusedBorderColor = "#DD4814" -- Ubuntu orange
	 ,  workspaces = myWorkspaces
	}





