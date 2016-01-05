module Color where

-- TODO: Add other file format(s).  (Currently using Netpbm P3 only.)
-- TODO: Compare performance of different color representations.
type Color = (Double, Double, Double)
type Pixel = (Int, Int, Int)

black   = (0.0, 0.0, 0.0)
white   = (1.0, 1.0, 1.0)

red     = (1.0, 0.0, 0.0)
green   = (0.0, 1.0, 0.0)
blue    = (0.0, 0.0, 1.0)

cyan    = (0.0, 1.0, 1.0)
magenta = (1.0, 0.0, 1.0)
yellow  = (1.0, 1.0, 0.0)

toPixel   (r, g, b) = (round(255*r), round(255*g), round(255*b))
showPixel (r, g, b) = (show r ++ " " ++ show g ++ " " ++ show b)