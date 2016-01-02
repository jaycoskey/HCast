module Image where

import App 
import Color 
import Screen 

ppm3Body :: Screen -> String
ppm3Body (Screen {width=w, height=h, colors=cs}) = unlines textRows
      where pixels :: [Pixel]
            pixels = map toPixel cs
            -- outputPixelRows = concat $ map S.chunksOf 5 $ S.chunksOf w pixels
            outputPixelRows = [pixels]
            showPixelRow ps = unwords $ map showPixel ps
            textRows = map showPixelRow outputPixelRows
 
ppm3Header :: AppConfig -> Screen -> String
ppm3Header (AppConfig { maxColorVal=max })
           (Screen {width=w, height=h, colors=_}) =
    "P3\n" ++ (show w) ++ " " ++ (show h) ++ "\n" ++ (show max) ++ "\n"

ppm3Text :: AppConfig -> Screen -> String
ppm3Text config screen =
    (ppm3Header config screen) ++ (ppm3Body screen)


