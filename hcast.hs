import qualified Data.List.Split as S
import Options.Applicative

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

greet :: AppOptions -> IO ()
greet (AppOptions name False) = putStrLn $ "Hello, " ++ name
greet _ = return ()

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

main :: IO ()
main = do
  let opts = info (helper <*> options)
            ( fullDesc
              <> progDesc "Print a greeting for TARGET"
              <> header "hello - a test for optparse-applicative"
            )
  execParser opts >>= greet
  let config = AppConfig { maxColorVal = 255 }
  let screen = Screen {
      width = 10
      ,height = 10
      ,colors = take 100 $ cycle [(0,0,0), (0,0,1), (0,1,0), (0,1,1)
                                 ,(1,0,0), (1,0,1), (1,1,0), (1,1,1)]
      }
  writeFile "output.ppm" (ppm3Text config screen)
