{ config, lib, pkgs,  ...}:

{

    home.packages = with pkgs; [
        xmonad-log
    ];

 xsession.windowManager.xmonad = {
     enable = true;
     enableContribAndExtras = true;
     config =  ./xmonad.hs;
     extraPackages = haskellPackages: [
        pkgs.haskellPackages.dbus
    ];
     libFiles = {
        "Config.hs" = lib/Config.hs;
     };
#     config = pkgs.writeText "xmonad.hs" ''
#     import XMonad
#
#     import XMonad.Util.EZConfig
#     import XMonad.Util.Ungrab
#     import XMonad.Actions.Navigation2D
#
#--     import XMonad.Hooks.EwmhDesktops
#
#     import XMonad.Layout.BinarySpacePartition
#     import XMonad.Layout.Magnifier
#     import XMonad.Hooks.ManageDocks
#
#     import XMonad.Layout.Gaps
#
#     import XMonad.Hooks.SetWMName
#
#     myLayout = tiled ||| Mirror tiled ||| Full ||| bsp
#      where
#     				tiled = Tall nmaster delta ratio
#     				bsp = magnifiercz' 1.3 $ emptyBSP
#     				nmaster = 1 -- Default number of windows in the main pane
#     				ratio = 1/2 -- proportion of the screen occupied by the main pane
#     				delta = 3/100 -- How much of the screen to change when resizing
#
#     bspKeys = navigation2DP def ("<Up>", "<Left>", "<Down>", "<Right>")
#                                 [ ("M-", windowGo)
#                                 , ("M-S-", windowSwap)
#                                 ]
#                                 False
#     main :: IO ()
#    -- main = xmonad $ ewmh $ bspKeys $  def
#     main = xmonad $ bspKeys $  def
#     				{ modMask = mod4Mask
#     				, manageHook = manageDocks <+> manageHook defaultConfig
#     				, layoutHook = gaps [(U,35), (D,5), (R,5), (L,5)] $ myLayout
#            , startupHook = setWMName "LG3D"
#     				}
#     				`additionalKeysP`
#     				[ ("M-<Return>", spawn "nixGL alacritty")
#     				, ("M-S-<Return>", spawn "rofi -show run")
#            , ("M-o", spawn "obsidian")
#            , ("M-r", sendMessage $ Rotate)
#     				]
#'';
   };
}
