import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Magnifier

myLayout = tiled ||| Mirror tiled ||| Full ||| bsp
 where
				tiled = Tall nmaster delta ratio
				bsp = magnifiercz' 1.3 $ emptyBSP
				nmaster = 1 -- Default number of windows in the main pane
				ratio = 1/2 -- proportion of the screen occupied by the main pane
				delta = 3/100 -- How much of the screen to change when resizing

main :: IO ()
main = xmonad $ ewmh $ def
				{ modMask = mod4Mask
				, layoutHook = myLayout
				}
				`additionalKeysP`
				[ ("M-<Return>", spawn "nixGL alacritty")
				, ("M-S-<Return>", spawn "rofi -show run")
				]
