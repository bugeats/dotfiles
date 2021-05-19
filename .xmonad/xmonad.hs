import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Hidden
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Gaps
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig ( additionalKeys )
import XMonad.Util.Replace ( replace )
import XMonad.Actions.GridSelect
import qualified Data.Map as M

windowMargin = 6

goldenRatio = toRational (2/(1+sqrt(5)::Double)) -- golden ratio
terminalCommand = "alacritty -e tmux new-session -A -s SCRATCH"

myLayout = gaps [(D, 32 + windowMargin)] $ spacingWithEdge windowMargin $ hiddenWindows $ ThreeColMid 1 (1/24) goldenRatio

myKeys = [ ((mod4Mask .|. shiftMask, xK_backslash), popOldestHiddenWindow)
         , ((mod4Mask .|. shiftMask, xK_q), spawn "xfce4-session-logout")
         , ((mod4Mask, xK_0), goToSelected defaultGSConfig)
         , ((mod4Mask, xK_b), sendMessage ToggleStruts)
         , ((mod4Mask, xK_backslash), withFocused hideWindow)
         , ((mod4Mask, xK_g), sendMessage ToggleGaps)
         , ((mod4Mask, xK_n), spawn terminalCommand) -- (n)ew terminal
         , ((mod4Mask, xK_p), spawn "xfce4-popup-whiskermenu")
         , ((mod4Mask, xK_s), spawn "systemctl suspend & slock")
         , ((mod4Mask, xK_space), spawn "xfce4-popup-whiskermenu")
         , ((mod4Mask, xK_x), spawn "slock")
         ]

main :: IO ()
main = do
  replace
  xmonad $ ewmh xfceConfig
    { borderWidth        = 6
    , modMask            = mod4Mask -- Use Super instead of Alt
    , terminal           = terminalCommand
    , focusedBorderColor = "#a57b55"
    , normalBorderColor  = "#272524"
    , layoutHook         = myLayout
    } `additionalKeys` myKeys

