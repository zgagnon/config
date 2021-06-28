{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Config where
import XMonad
import XMonad.Util.EZConfig
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.Navigation2D
import XMonad.Actions.Submap
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place

import qualified XMonad.Layout.BoringWindows as B
import XMonad.Layout.LayoutModifier (ModifiedLayout(..))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Spacing

import System.Exit
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Ratio ((%))

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

modMask' :: KeyMask
modMask' = mod4Mask

delta :: Rational
delta = 3 / 100

fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"

bg1       = "#389"
bg2       = "#8bc"
bg3       = "#755"
bg4      = "#edc"
bg5       = "#cbc"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"

myLayouts = renamed [CutWordsLeft 1] .
    avoidStruts .  B.boringWindows $
    smartBorders $
    spacing 5
        ( aTiled
        ||| aFullscreen
        ||| aTabbed
        ||| Mirror aTiled
        ||| emptyBSP
        )
  where
    aTabbed = renamed [Replace "Tab"] $ simpleTabbed
    aFullscreen = renamed [Replace "Full"] $ noBorders Full
    aTiled = renamed [Replace "Tile"] $ Tall 1 (3 / 100) (1 / 2)
    defTabbed = def
        { activeColor = bg
        , urgentColor = red
        , inactiveColor = bg
        , activeBorderColor = bg
        , inactiveBorderColor = bg
        , urgentBorderColor = red
        , inactiveTextColor = gray -- Gray color on dark gray background
        , activeTextColor = green
        , urgentTextColor = "#ffffff"
        , fontName = "xft:Liberation Sans:size=10" }

switchWorkspaceToWindow :: Window -> X ()
switchWorkspaceToWindow w = windows $ do
    tag <- W.currentTag
    W.focusWindow w . W.greedyView tag . W.focusWindow w

workspaces' = ["1:web", "2:code", "3:media", "4:im", "5", "6", "7", "8", "9"]

myManageHook = composeAll
    [ className =? "MPlayer"          --> doFloat
    , className =? "Gimp"             --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore
    -- Flash :(
    , className =? "Plugin-container" --> doFloat
    , className =? "mpv"              --> doFloat
    , className =? "feh"              --> doFloat
    , className =? "keepassx"         --> doFloat
    , className =? "Gpick"            --> doFloat
    , className =? "Thunar"           --> doFloat
    , className =? "Qalculate-gtk"    --> doFloat
    , className =? "Pcmanfm"          --> doFloat
    , className =? "Dunst"          --> doFloat
    -- Used by Chromium developer tools, maybe other apps as well
    , role =? "pop-up"                --> doFloat ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

myManageHook' = composeOne [ isFullscreen -?> doFullFloat ]

newKeys = \c -> mkKeymap c $
    -- Launchers
    [ ("M-S-<Return>", spawn "rofi -show combi -combi-modi run,drun")
    , ("M-<Return>", spawn $ terminal c)
    , ("M-S-<Backspace> p", spawn "pavucontrol")
    , ("M-S-<Backspace> v", spawn "vivaldi")
    , ("M-S-<Backspace> o", spawn "obsidian")
		, ("M-S-<Backspace> s", spawn "flameshot gui")
    -- Window arrangement
    , ("M-S-m", windows W.swapMaster)
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-r", sendMessage Rotate)
    , ("M-S-c", sendMessage SelectNode)
    , ("M-S-v", sendMessage MoveNode)
    -- Push back in to tiling
    , ("M-t", withFocused $ windows . W.sink)
    -- Window Sizing
    , ("M-S-<Down>", sendMessage $ ShrinkFrom U)
    , ("M-S-<Up>", sendMessage $ ExpandTowards U)
    , ("M-S-<Right>", sendMessage $ ExpandTowards R)
    , ("M-S-<Left>", sendMessage $ ShrinkFrom R)
    -- Navigation
    , ("M-<Left>", windowGo L False)
    , ("M-<Right>", windowGo R False)
    , ("M-<Up>", windowGo U False)
    , ("M-<Down>", windowGo D False)
    , ("M-w", kill)
    -- Workspaces
    ]
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
--    [((m .|. modm, k), windows $ f i)
--        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_F9), spawn "kodi")
    , ((modm, xK_r), spawn "rofi -show run -switchers 'run,window' -no-levenshtein-sort")

    -- Lock screen
    , ((modm, xK_a), submap . M.fromList $
        [ ((0, xK_l), spawn "lock.sh") ])

    -- close focused window
    , ((modm, xK_w), kill)
     -- Rotate through the available layout algorithms
    , ((modm, xK_space), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm, xK_n), refresh)
    -- Move focus to the next window
    , ((modm, xK_Tab), B.focusDown)
    -- Move focus to the next window
    , ((modm, xK_j), B.focusDown)
    -- Move focus to the previous window
    , ((modm, xK_k), B.focusUp)
    -- Move focus to the master window
    , ((modm, xK_m), B.focusMaster)
    -- Swap the focused window with the master window
    , ((modm .|. shiftMask, xK_m), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    -- Shrink the master area
    , ((modm, xK_h), sendMessage Shrink)
    -- Expand the master area
    , ((modm, xK_l), sendMessage Expand)
    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q), io exitSuccess)
    -- Restart xmonad
    , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")

    -- 2D navigation
    , ((modm .|. shiftMask, xK_l), screenGo R True)
    , ((modm .|. shiftMask, xK_h), screenGo L True)
    , ((modm .|. controlMask, xK_l), screenSwap R True)
    , ((modm .|. controlMask, xK_h), screenSwap L True)

    -- Float handling (snapping to edges)
    , ((modm, xK_Right), withFocused $ snapMove R Nothing)
    , ((modm, xK_Left), withFocused $ snapMove L Nothing)
    , ((modm, xK_Up), withFocused $ snapMove U Nothing)
    , ((modm, xK_Down), withFocused $ snapMove D Nothing)

    , ((modm .|. shiftMask, xK_Right), withFocused $ keysResizeWindow (20, 0) (0, 0))
    , ((modm .|. shiftMask, xK_Left), withFocused $ keysResizeWindow (-20, 0) (0, 0))
    , ((modm .|. shiftMask, xK_Up), withFocused $ keysResizeWindow (0, -20) (0, 0))
    , ((modm .|. shiftMask, xK_Down), withFocused $ keysResizeWindow (0, 20) (0, 0))

    -- Struts...
    , ((modm .|. controlMask, xK_0), sendMessage $ ToggleStrut U)
    ]
    ++
    -- Media hotkeys
    [((mod5Mask, k), spawn $ "playerctl " ++ m)
        | (m, k) <- zip ["previous", "play-pause", "next"] [xK_3..xK_5]]
    ++
    [((noModMask, k), spawn $ "playerctl " ++ m)
        | (m, k) <-
            [ ("previous", xF86XK_AudioPrev)
            , ("play-pause", xF86XK_AudioPlay)
            , ("next", xF86XK_AudioNext) ]]
    ++
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{F" ++ bg3 ++ "} ") ("%{F" ++ bg4 ++ "} ")
    , ppVisible = wrap ("%{F" ++ bg4 ++ "} ") ""
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap "" " "
    , ppWsSep = ""
    , ppSep = " : "
    , ppTitle = shorten 40
    , ppLayout = wrap ("%{F" ++ bg4 ++ "} ") ""
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

createConfig localTerminal =  def
    { terminal = localTerminal
    , layoutHook = myLayouts
    , manageHook = placeHook (smart (0.5, 0.5))
                    <+> manageDocks
                    <+> myManageHook
                    <+> myManageHook'
                    <+> manageHook def
    , handleEventHook = docksEventHook <+> minimizeEventHook <+> fullscreenEventHook
    , keys = newKeys
    -- Don't be stupid with focus
    , clickJustFocuses = False
    , borderWidth = 3
    , normalBorderColor = gray
    , focusedBorderColor = "#00FF00"
    , workspaces = workspaces'
    , modMask = modMask' }
