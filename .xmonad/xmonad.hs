import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

main :: IO ()
main = do
  xmonad $ defaultConfig
    { borderWidth        = 1
    , terminal           = "alacritty"
    , focusedBorderColor = "#645851"
    , normalBorderColor  = "#272524"
    , layoutHook         = myLayout
    }

ratio = toRational (2/(1+sqrt(5)::Double)) -- golden ratio
mySpacing = 16
myLayout = spacingWithEdge mySpacing $ Tall 1 (3/100) ratio
