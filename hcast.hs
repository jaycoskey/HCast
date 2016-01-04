
import Geometry

import Data.List (sortBy)
import Data.Maybe (catMaybes)
-- import Debug.Trace (trace)
import Options.Applicative 
import Test.HUnit (runTestTT)

import App
import Camera
import Color
import Image
import Light
import Object
import Scene
import SceneExpr
import Screen
import Shape

type Depth = Int

debugShowScreenCoords w h i j = "Width: "     ++ (show i) ++ "/" ++ (show w) ++ "; "
                                ++ "Height: " ++ (show j) ++ "/" ++ (show h)

debugDecoratedShow pre a post =
    pre ++ (debugShowVec3 a) ++ post

options :: Parser AppOptions
options = 
    AppOptions
      <$> strOption
        ( long "output"
          <> short 'o' 
          -- <> metavar "Output PPM file"
          -- <> help "Output of the scene rendering"
        )
      <*> option auto
        ( long "width"
          <> short 'w'
          -- <> metavar "Width of the output file, in pixels "
          -- <> help "Width of the output file"
        )
      <*> option auto
        ( long "height"
          <> short 'h'
          -- <> metavar "Height of the output file, in pixels "
          -- <> help "Height of the output file"
        )


getColor :: Scene -> Camera -> Depth
         -> Int -> Int -> Int -> Int -> Color
getColor scene cam d w h i j =
    case maybeIntersection of
        Just intersection -> getCombinedColor intersection
        Nothing           -> black
      where 
        maybeIntersection = getFirstIntersection scene cam d w h i j
        getPointColor intersection@Intersection 
                             { ray=_
                             , distance=_
                             , point=_
                             , normal=_
                             , color=pointColor
                             , material=_
                             }
                          = pointColor
        reflectedRay =
        getReflectedColor intersection@Intersection 
                             { ray=_
                             , distance=_
                             , point=_
                             , normal=_
                             , color=pointColor
                             , material=_
                             }
                          = getColor scene cam (d+1) w h i j
        getRefractedColor intersection@Intersection 
                             { ray=_
                             , distance=_
                             , point=_
                             , normal=_
                             , color=pointColor
                             , material=_
                             }
                          = getColor scene cam (d+1) w h i j
        refractedRay =
        getCombinedColor intersection@Intersection
                             { ray=_
                             , distance=_
                             , point=_
                             , normal=_
                             , color=pointColor
                             , material=_
                             }
                          = getColor scene cam (d+1) w h i j

getFirstIntersection :: Scene -> Camera -> Depth 
                     -> Int -> Int -> Int -> Int -> Maybe Intersection
getFirstIntersection scene cam d w h i j =
    if (length intersections == 0)
    then Nothing
    else Just $ head (sortBy cmpDistance intersections)
      where
        intersections = getIntersections scene cam w h i j


getIntersections :: Scene -> Camera -> Depth
                 -> Int -> Int -> Int -> Int -> [Intersection]
getIntersections 
    (Scene  { objects=objs, lights=lux })
    (Camera  { posn=camPos, dirn=camDir, up=camUp, hFov=camHFov })
    w h i j
    = catMaybes maybeIntersections
        where
          h_d = (fromIntegral h) :: Double 
          w_d = (fromIntegral w) :: Double
          i_d = (fromIntegral i) :: Double 
          j_d = (fromIntegral j) :: Double
          hFrac = (i_d - (w_d/2.0)) / w_d
          vFrac = (j_d - (h_d/2.0)) / h_d
          camVFov   = (h_d / w_d) * camHFov
          panAngle  = hFrac * camHFov
          tiltAngle = vFrac * camVFov
          camLeft   = camUp >< camDir
          tmpDir    = rotateAroundAxisByAngle camUp   panAngle  camDir
          eyeDir    = rotateAroundAxisByAngle camLeft tiltAngle tmpDir
          ray       = Ray { orig = camPos, dir = eyeDir }
          debugText = (debugShowScreenCoords w h i j) ++ "\n"
                        ++ (debugDecoratedShow "Ray: orig=" camPos "; ")
                        ++ (debugDecoratedShow       "dir=" eyeDir "\n")
          maybeIntersections = map (\obj -> intersect obj ray) objs

scene = Scene { objects  = [ mkObject (Sphere (-2.0,-2.0, 2.0) 1.5) red
                           , mkObject (Sphere (-2.0, 2.0, 2.0) 1.5) green
                           , mkObject (Sphere ( 2.0,-2.0, 2.0) 1.5) blue
                           , mkObject (Sphere ( 0.0, 0.0, 4.0) 1.5) white
                           , mkObject (Plane  (0, 0, 0) (0, 0, 1))  red
                           ]
              , lights   = [PointLight { position =( 5.0,  5.0, 2.0)
                                       , direction=(-5.0, -5.0, 0.0)
                                       }
                           ]
              , ambient  = (0.5, 0.0, 0.0) 
              }

camera = Camera { posn = ( 0.0, 0.0, 10.0 )
                , dirn = ( 0.0, 0.0, -1.0 )
                , up   = ( 1.0, 0.0,  0.0 )
                , hFov = 60 * degrees
                }

hCast :: AppOptions -> IO()
hCast AppOptions { appOFile=ofile, appWidth=w, appHeight=h }
  = writeFile ofile (ppm3Text config screen)
      where
        config = AppConfig { maxColorVal = 255 }
        screen = -- trace debugShowCrossProducts -- debugShowRotations
                 Screen { width  = w
                        , height = h
                        , colors = concat
                                     [
                                       [ getColor scene camera w h i j
                                           | i <- [0 .. (w - 1)]
                                       ]   | j <- [0 .. (h - 1)]
                                     ]
                        }

main :: IO ()
main = do
    runTestTT testCrossProdList
    runTestTT testRotateList 
    -- putStrLn "testCrossProdX"
    -- putStrLn $ show $ eqVec3Eps (yU >< zU) xU eps
    -- putStrLn "testCrossProdY"
    -- putStrLn $ show $ eqVec3Eps (zU >< xU) yU eps
    -- putStrLn "testCrossProdZ"
    -- putStrLn $ show $ eqVec3Eps (xU >< yU) zU eps

    let opts = info (helper <*> options)
               ( fullDesc
                 <> progDesc "Render a scene using ray tracing"
                 <> header "When is this text displayed?"
               )
    execParser opts >>= hCast

