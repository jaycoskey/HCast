-- {-# LANGUAGE : TODO #-}

import Data.Function  -- For the definition of `on`
import Options.Applicative

-- ===== AppOptions =====
data AppOptions = AppOptions
  { ofile :: String
  , quiet :: Bool
  }

-- ===== Color =====
-- TODO: Netpbm P3 for prototype.  Move to other file format(s) later.
-- TODO: Compare performance of different color representations.
type Color = (Double, Double, Double)
type Pixel = (Int, Int, Int)

toPixel (r, g, b) = (round(255*r), round(255*g), round(255*b))
showPixel (r, g, b) = (show r ++ " " ++ show g ++ " " ++ show b)

-- ===== AppConfig =====
data AppConfig = AppConfig
  { maxColorVal :: Int
  }

-- ===== Screen =====
data Screen = Screen
  { width :: Int
  , height :: Int
  , colors :: [Color]
  }

-- ===== Image =====
ppm3Body :: Screen -> String
ppm3Body screen@(Screen {width=w, height=h, colors=cs}) = unlines textRows
      where pixels :: [Pixel]
            pixels = map toPixel cs 
            outputPixelRows = [pixels] -- TODO: concat $ map S.chunksOf 5 $ S.chunksOf w pixels
            showPixelRow ps = unwords $ map showPixel ps
            textRows = map showPixelRow outputPixelRows
 
ppm3Header :: AppConfig -> Screen -> String
ppm3Header config@(AppConfig { maxColorVal=max })
           screen@(Screen {width=w, height=h, colors=_}) =
    "P3\n" ++ (show w) ++ " " ++ (show h) ++ "\n" ++ (show max) ++ "\n"

ppm3Text :: AppConfig -> Screen -> String
ppm3Text config screen =
    (ppm3Header config screen) ++ (ppm3Body screen)

-- ===== HCastrMain =====
options :: Parser AppOptions
options = AppOptions
    <$> strOption
        ( long "ofile"
          <> metavar "Output PPM file"
          <> help "Output of the scene rendering"
        )
    <*> switch
        ( long "quiet"
          <> help "Omit logging to standard output"
        )

-- TODO: Move scene information to an input file, parameterize output file, etc.
main :: IO ()
main = do
  let opts = info (helper <*> options)
             (  fullDesc
             <> progDesc "Render a scene using ray tracing"
             <> header "When is this text displayed?"
             )
  execParser opts
  let config = AppConfig { maxColorVal = 255 }
  let w = 100
  let h = 10
  let div = (/) `on` fromIntegral
  let getColor i j = (i `div` (w - 1), 0.5, j `div` (h - 1))
  let screen = Screen
                 { width = w
                 , height = h
                 , colors = concat
                              $ [ [getColor i j | i <- [0 .. (w - 1)]]
                                | j <- [0 .. (h - 1)]
                                ]
                 }
  writeFile "output.ppm" (ppm3Text config screen)
