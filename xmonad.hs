import           XMonad
import           System.Exit
import           System.IO
import           Graphics.X11.ExtraTypes.XF86
import           Control.Monad

import qualified XMonad.StackSet               as W

import           Data.Maybe
import           Data.List
import           Data.Monoid

import           XMonad.Util.SpawnOnce          ( spawnOnce )
--import           XMonad.Util.Run
import           XMonad.Util.EZConfig           ( additionalKeys )
import           XMonad.Util.Ungrab

import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageHelpers     ( isFullscreen )
import           XMonad.Hooks.InsertPosition

import           XMonad.Layout.NoBorders
import           XMonad.Layout.Fullscreen


{-----------------------------------------------------}

main :: IO ()
main = do
  xmonad =<< xmobar defaults


-- Settings and keybindings
defaults =
  ewmh $ docks def { terminal        = "kitty" -- kitty as default terminal
                   , modMask         = mod4Mask -- Super key as Mod key
                   , startupHook     = myStartupHook <+> setFullscreenSupported
                   , layoutHook      = myLayoutHook
                   , manageHook      = insertPosition End Older
                                       <+> manageDocks
                                       <+> fullscreenManageHook
                                       <+> myManageHook
                   , logHook         = dynamicLog
                   , handleEventHook = handleEventHook def
                                       <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook
                   }
    `additionalKeys` {- My Keybindings -}
                     [
                     -- default app launcher
                       ( (modm, xK_p)
                       , spawn "rofi -show drun -theme gruvbox-dark-hard"
                       )
                     -- Volume Control
                     , ( (0, xF86XK_AudioMute)
                       , spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
                       )
                     , ( (0, xF86XK_AudioLowerVolume)
                       , spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"
                       )
                     , ( (0, xF86XK_AudioRaiseVolume)
                       , spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
                       )
                     -- Brightness Control
                     , ((0, xF86XK_MonBrightnessUp)
                       , spawn "xbacklight +5")
                     , ( (0, xF86XK_MonBrightnessDown)
                       , spawn "xbacklight -5"
                       )
                     -- Screenshot
                     , ( (0, xK_Print)
                       , spawn "scrot -q 90 /home/thonkpad/Pictures/scrot-%F-%T.png"
                       )
                     , ( (shiftMask, xK_Print)
                       , unGrab >> spawn "scrot -q 90 -s /home/thonkpad/Pictures/scrot-%F-%T.png"
                       )
                     ]
  where modm = mod4Mask

-- Programs to start upon logging in to Xmonad
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom &"
  spawnOnce "dunst &"
  spawnOnce "xscreensaver &"
  spawnOnce "stalonetray &"
  spawnOnce "nm-applet &"
  spawnOnce "udiskie -ant &"

-- Tiling layouts for Xmonad
myLayoutHook = smartBorders $ avoidStruts (tiled ||| Mirror tiled ||| Full)
 where
    -- default tiling algorithm partitions the screen into two panes
  tiled   = Tall nmaster delta ratio
  -- The default number of windows in the master pane
  nmaster = 1
  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2
  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

-- What to do with certain programs upon executing
-- `doFloat` automatically sends the program to the floating layer
myManageHook = composeAll
  [ className =? "mpv" --> doFloat
  , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
  , className =? "nm-applet" --> doFloat
  , className =? "stalonetray" --> doIgnore
  , isFullscreen --> doFullFloat
  ]



-- THIS IS FOR FULLSCREEN SUPPORT
-- source: https://www.reddit.com/r/xmonad/comments/gc4b9i/what_is_the_best_way_to_make_xmonad_respect_true/fpbnsv9/
setFullscreenSupported :: X ()
setFullscreenSupported =
  addSupported ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"]

addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
  r                <- asks theRoot
  a                <- getAtom "_NET_SUPPORTED"
  newSupportedList <- mapM (fmap fromIntegral . getAtom) props
  io $ do
    supportedList <- fmap (join . maybeToList) $ getWindowProperty32 dpy a r
    changeProperty32 dpy
                     r
                     a
                     aTOM
                     propModeReplace
                     (nub $ newSupportedList ++ supportedList)
