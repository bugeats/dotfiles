import XMonad
import XMonad.Layout.Spacing

main :: IO ()
main = do
  xmonad $ defaultConfig
    { borderWidth        = 1
    , terminal           = "alacritty"
    , focusedBorderColor = "#645851"
    , normalBorderColor  = "#272524"
    , layoutHook         = spacingWithEdge mySpacing $ Tall 1 (3/100) ratio
    }
  where
    ratio = toRational (2/(1+sqrt(5)::Double)) -- golden ratio

mySpacing = 16
