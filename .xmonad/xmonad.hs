import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS

-- Great intro: http://www.linuxandlife.com/2011/11/how-to-configure-xmonad-arch-linux.html

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-launcher" --> doIgnore
    , className =? "Gimp-2.6" --> doFloat
    -- , className =? "Empathy" --> doShift "3:chat"
    -- , className =? "Emacs" --> doShift "2:emacs"
    -- , className =? "Firefox" --> doShift "1:web"
    ])

myWorkspaces = ["1:web","2:emacs","3:chat","4:docs","5","6","7","8","9"]  

main = xmonad $ gnomeConfig 
	{ manageHook = myManageHook 
         , modMask = mod4Mask
         , focusedBorderColor = "#DD4814" -- Ubuntu orange
	 , workspaces = myWorkspaces
	} `additionalKeysP`
        [ ("M-f", goToSelected defaultGSConfig)
           , ("M-s", spawnSelected defaultGSConfig ["emacs","firefox"])
           -- Parece que no esta disponible  ("M-w",  gridselectWorkspace defaultGSConfig)
           ,("M-<L>", prevWS)
           ,("M-<R>", nextWS) 
        ]



