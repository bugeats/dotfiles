import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig ( additionalKeys )
import qualified Data.Map as M

main :: IO ()
main = do
  xmonad $ defaultConfig
    { borderWidth        = 1
    , terminal           = "alacritty"
    , focusedBorderColor = "#645851"
    , normalBorderColor  = "#272524"
    , layoutHook         = myLayout
    } `additionalKeys` myKeys

goldenRatio = toRational (2/(1+sqrt(5)::Double)) -- golden ratio

mySpacing = 16

myLayout = spacingWithEdge mySpacing $ ThreeColMid 1 (1/12) goldenRatio

myKeys = [ ((mod1Mask, xK_x), spawn "slock") ]
