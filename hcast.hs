-- For future performance improvements:
-- {-# LANGUAGE BangPatterns #-}

-- Compiler flags to keep in mind:
--   -threaded -O2 -funbox-strict-fields -fexcess-precision
--   -funfolding-keeness-factor=10 -fvia-C -optc-O3
--   -optc-msse3 -optc-ffast-math

-- import System.IO

import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Debug.Trace
import Numeric
import Options.Applicative
import Test.HUnit
import Test.HUnit.Text

-- ===== AppConfig =====
data AppConfig = AppConfig
  { maxColorVal :: Int
  }

-- ===== AppOptions =====
data AppOptions = AppOptions
  { appOFile  :: String
  , appWidth  :: Int
  , appHeight :: Int
  -- , quiet :: Bool
  }

-- ===== Camera =====
-- TODO: Orthographic, Perspective, Fisheye cameras, etc.
data Camera = Camera
  { posn :: Pos3f
  , dirn :: Vec3f  -- lookAt :: Pos3f
  , up   :: Vec3f
  , hFov :: Double
  }

-- ===== Color =====
-- TODO: Netpbm P3 for prototype.  Possibly move to other file format(s) later.
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

toPixel (r, g, b) = (round(255*r), round(255*g), round(255*b))
showPixel (r, g, b) = (show r ++ " " ++ show g ++ " " ++ show b)

-- ===== Geometry =====
type Pos3f = (Double, Double, Double)
type Vec3f = (Double, Double, Double)

xUnit = (1.0, 0.0, 0.0)
yUnit = (0.0, 1.0, 0.0)
zUnit = (0.0, 0.0, 1.0)
zero3 = (0.0, 0.0, 0.0)

xU = xUnit
yU = yUnit
zU = zUnit
z3 = zero3

eqVec3fEps :: Vec3f -> Vec3f -> Double -> Bool
eqVec3fEps v1 v2 epsilon = norm2 (v1 <-> v2) < epsilon

degrees = pi / 180.0

debugShowVec3f (a, b, c) = "(" 
                       ++ (showFFloat (Just 2) a "") ++ ", "
                       ++ (showFFloat (Just 2) b "") ++ ", "
                       ++ (showFFloat (Just 2) c "") 
                       ++ ")"

data Ray = Ray
  { orig :: Pos3f
  , dir  :: Vec3f
  } deriving Show

-- TODO! Set fixity
(.^) :: Double -> Vec3f -> Vec3f
(.^) d (x, y, z) = (d*x, d*y, d*z)

dot :: Vec3f -> Vec3f -> Double
dot (x1, y1, z1) (x2, y2, z2) = (x1*x2 + y1*y2 + z1*z2)

norm2 :: Vec3f -> Double
norm2 vec = vec `dot` vec

norm :: Vec3f -> Double
norm vec = sqrt $ norm2 vec

normalize :: Vec3f -> Vec3f
normalize vec = (1 / (norm vec)) .^ vec

-- TODO! Set fixity
(<+>) :: Pos3f -> Vec3f -> Vec3f
(<+>) (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

(<->) :: Pos3f -> Vec3f -> Vec3f
(<->) (x1, y1, z1) (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)

-- Borrow operator from Data.Vec3.
(><) :: Vec3f -> Vec3f -> Vec3f
(><) (x1, y1, z1) (x2, y2, z2)
    = ( y1 * z2 - y2 * z1
      , x2 * z1 - x1 * z2
      , x1 * y2 - x2 * y1)

rayAtTime :: Ray -> Double -> Pos3f
rayAtTime (Ray orig dir) t = orig <+> (t .^ dir)

debugShow2 x = showFFloat (Just 2) x ""

-- Rodrigues' rotation formula
-- TODO: Possibly switch to quaternions to represent rotations.
rotateAroundAxisByAngle axis angle v =
    -- trace debugStr result
    result
    where c      = cos angle
          s      = sin angle
          nAxis  = normalize axis -- TODO: Normalize in caller if more efficient.
          dp     = nAxis `dot` v -- dp = dot product
          cp     = nAxis >< v    -- cp = cross product
          result = (c              .^ v)
                   <+> (((1-c)*dp) .^ nAxis)
                   <+> (s          .^ cp)
          debugStr = "Rot: "
              ++   "c="      ++ (debugShow2 c)
              ++ "; s="      ++ (debugShow2 s)
              ++ "; nAxis="  ++ (debugShowVec3f nAxis)
              ++ "; dp="     ++ (debugShow2 dp)
              ++ "; cp="     ++ (debugShowVec3f cp)
              ++ "; v="      ++ (debugShowVec3f v)
              ++ "; result=" ++ (debugShowVec3f result)
              ++ "\n"

eps = 0.0001

-- TODO: Use Test.HUnit.Approx
testCrossProdX = TestCase $ assertBool "testCrossProdX" $ eqVec3fEps (yU >< zU) xU eps
testCrossProdY = TestCase $ assertBool "testCrossProdY" $ eqVec3fEps (zU >< xU) yU eps
testCrossProdZ = TestCase $ assertBool "testCrossProdZ" $ eqVec3fEps (xU >< yU) zU eps
testCrossProdList = TestList [ TestLabel "testCrossProdX" testCrossProdX
                             , TestLabel "testCrossProdY" testCrossProdY
                             , TestLabel "testCrossProdZ" testCrossProdZ
                             ]

testRotateX = TestCase $ assertBool "testRotateX" $ eqVec3fEps (yU >< zU) xU eps
testRotateY = TestCase $ assertBool "testRotateY" $ eqVec3fEps (zU >< xU) yU eps
testRotateZ = TestCase $ assertBool "testRotateZ" $ eqVec3fEps (xU >< yU) zU eps
testRotateList = TestList [ TestLabel "testRotateX" testRotateX
                          , TestLabel "testRotateY" testRotateY
                          , TestLabel "testRotateZ" testRotateZ
                          ]

debugShowCrossProduct v1 v2 = testResultStr
  where
    result    = v1 >< v2
    v1Str     = debugShowVec3f v1
    v2Str     = debugShowVec3f v2
    resultStr = debugShowVec3f result
    testResultStr = "Cross product of " ++ (debugShowVec3f v1)
                    ++ " with "         ++ (debugShowVec3f v2)
                    ++ " = "            ++ (debugShowVec3f result)

debugShowCrossProducts = testResultStr
  where
    result1 = debugShowCrossProduct yUnit zUnit  -- Result = xUnit
    result2 = debugShowCrossProduct zUnit xUnit  -- Result = yUnit
    result3 = debugShowCrossProduct xUnit yUnit  -- Result = zUnit
    testResultStr = result1 ++ result2 ++ result3

debugShowRotation axis deg v = testResultStr
  where
    angle     = deg * degrees
    axisStr   = debugShowVec3f axis
    vStr      = debugShowVec3f v
    result    = rotateAroundAxisByAngle axis angle v
    resultStr = debugShowVec3f result
    testResultStr = "Result of rotating " ++ vStr
                    ++ " around " ++ axisStr
                    ++ " through an angle of " ++ (show deg) ++ " degrees"
                    ++ " is " ++ resultStr
                    ++ "\n"

debugShowRotations = testResultString
  where
    result1 = debugShowRotation yUnit 90 zUnit  -- Result = xUnit
    result2 = debugShowRotation zUnit 90 xUnit  -- Result = yUnit
    result3 = debugShowRotation xUnit 90 yUnit  -- Result = zUnit
    testResultString = result1 ++ result2 ++ result3

-- ===== Image =====
ppm3Body :: Screen -> String
ppm3Body screen@(Screen {width=w, height=h, colors=cs}) = unlines textRows
      where pixels :: [Pixel]
            pixels = map toPixel cs
            -- outputPixelRows = concat $ map S.chunksOf 5 $ S.chunksOf w pixels
            outputPixelRows = [pixels]
            showPixelRow ps = unwords $ map showPixel ps
            textRows = map showPixelRow outputPixelRows
 
ppm3Header :: AppConfig -> Screen -> String
ppm3Header config@(AppConfig { maxColorVal=max })
           screen@(Screen {width=w, height=h, colors=_}) =
    "P3\n" ++ (show w) ++ " " ++ (show h) ++ "\n" ++ (show max) ++ "\n"

ppm3Text :: AppConfig -> Screen -> String
ppm3Text config screen =
    (ppm3Header config screen) ++ (ppm3Body screen)

-- ===== Light =====
type Fov = Double

data Light = AmbientLight Color
           | Spotlight    Color Pos3f Vec3f
           | PointLight   Color Pos3f

-- ===== Math =====
minQuadraticRoot :: Double -> Double -> Double -> Maybe Double
minQuadraticRoot a b c
    | (isZero a) && (isZero b) = Nothing
    | (isZero a) && (isPos  b) = Just (-c / b)
    | isPos disc               = Just (min ((-b+sqrtDisc)/(2*a))
                                           ((-b-sqrtDisc)/(2*a))
                                      )
    | otherwise                = Nothing
    where disc     = b**2 - 4*a*c
          sqrtDisc = sqrt disc
          eps      = 0.0001  -- TODO: Revisit
          isPos x  = x > eps
          isZero x = abs x < eps

-- ===== Object =====
type DummyType = Int             -- TODO: Remove placeholder after features implemented.
type Material = Color            -- TODO: Implement
type Transformation = DummyType  -- TODO: Implement
dummyTransformation = 1 :: Transformation

type Distance = Double
type Time     = Double
data Intersection = Intersection
  { ray      :: Ray
  , distance :: Distance
  , point    :: Pos3f 
  , normal   :: Vec3f
  , color    :: Color
  -- , material :: Material
  }

cmpDistance :: Intersection -> Intersection -> Ordering
cmpDistance int1@Intersection { ray=_
                              , distance=dist1
                              , point=_
                              , normal=_
                              , color=_
                              }
            int2@Intersection { ray=_
                              , distance=dist2
                              , point=_
                              , normal=_
                              , color=_
                              }
    | dist1 < dist2 = LT
    | otherwise     = GT

data Object = Object Shape Material Transformation -- Intersection

-- TODO!  Remove hack
mkObject :: Shape -> Color -> Object
mkObject shape color = Object shape color dummyTransformation -- dummyIntersection

-- ===== Scene =====
-- TODO: Convert to scene graph, as in Coin3D and FEI's OpenInventor.
-- TODO: Efficiency: Groups w/ bounding spheres, kd-trees, etc.
data Scene = Scene
  { objects :: [Object]
  , lights  :: [Light]
  }

-- ===== Screen =====
-- TODO: Add super-sampling in this record,
--       and add a reduction step between Screen and output Image.
data Screen = Screen
  { width :: Int
  , height :: Int
  , colors :: [Color]  -- TODO: Use Array representation
  }

-- ===== Shape =====
type Radius = Double
type Normal = Vec3f
type Axis   = Vec3f
type Mesh   = Int  -- TODO: Implement

data Shape = Sphere       Pos3f Radius
           | Plane        Pos3f Normal

           | Cone         Pos3f Radius Axis
           | Cylinder     Pos3f Radius Axis
           | PolygonMesh  Pos3f Mesh  -- For now, Solid object only
           | Torus        Pos3f Float Float  -- major radius, minor radius

           | Tetrahedron  Pos3f  -- TODO: Document default orientation
           | Cube         Pos3f  -- TODO: Document default orientation
           | Octahedron   Pos3f  -- TODO: Document default orientation
           | Dodecahedron Pos3f  -- TODO: Document default orientation
           | Icosahedron  Pos3f  -- TODO: Document default orientation

           -- TODO: Move CSG feature into Scene graph
           | CsgDiff      Shape Shape
           | CsgSymDiff   Shape Shape
           | CsgUnion     Shape Shape

isBounded :: Shape -> Bool
isBounded (Plane _ _) = False
isBounded _ = True

isSolid :: Shape -> Bool
isSolid (Plane _ _) = False
isSolid _ = True

intersect :: Object -> Ray -> Maybe Intersection
intersect
    obj@( Object
              shape@(Sphere center rad)
              objColor
              -- objMaterial
              objXform
        )
    ray@(Ray { orig=orig, dir=dir })
      = case (maybeIntersectTime) of
          Just time -> if time > 0
                       then Just Intersection
                            { ray      = ray
                            , distance = time * norm(dir)
                            , point    = intersectionPoint
                            , normal   = normalize(intersectionPoint <-> center)
                            , color    = objColor
                            }
                       else Nothing
                       where
                           intersectionPoint  = center <+> (time .^ dir)
          Nothing   -> Nothing
        where
          delta = orig <-> center
          a = dir `dot` dir
          b = 2 * (dir `dot` delta)
          c = (delta `dot` delta) - rad*rad
          maybeIntersectTime = minQuadraticRoot a b c

intersect
    obj@(Object (Plane pos norm)
                objColor
                -- objMaterial
                objXform
        )
    (Ray { orig=orig, dir=dir }) =
        Nothing  -- TODO: Complete

intersect _ _ = Nothing

-- ===== HCastrMain =====
debugShowScreenCoords w h i j = "Width: "     ++ (show i) ++ "/" ++ (show w) ++ "; "
                                ++ "Height: " ++ (show j) ++ "/" ++ (show h)

debugDecoratedShow pre a post =
    pre ++ (debugShowVec3f a) ++ post

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

getColor :: Scene -> Camera -> Int -> Int -> Int -> Int -> Color
getColor scene cam w h i j =
    case maybeIntersection of
        Just Intersection { ray=_
                          , distance=_
                          , point=_
                          , normal=_
                          , color=color
                          } -> color
        Nothing    -> black
      where 
        maybeIntersection = getFirstIntersection scene cam w h i j

getFirstIntersection :: Scene -> Camera -> Int -> Int -> Int -> Int -> Maybe Intersection
getFirstIntersection scene cam w h i j =
    if (length intersections == 0)
    then Nothing
    else Just $ head (sortBy cmpDistance intersections)
      where
        intersections = getIntersections scene cam w h i j

getIntersections :: Scene -> Camera -> Int -> Int -> Int -> Int -> [Intersection]
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

scene = Scene { objects = [ mkObject (Sphere (-2.0,-2.0, 2.0) 1.5) red
                          , mkObject (Sphere (-2.0, 2.0, 2.0) 1.5) green
                          , mkObject (Sphere ( 2.0,-2.0, 2.0) 1.5) blue
                          , mkObject (Sphere ( 2.0, 2.0, 2.0) 1.5) white
                          , mkObject (Plane  (0, 0, 0) (0, 0, 1))  red
                          ]
              , lights = []
              }

camera = Camera
  { posn = ( 0.0, 0.0,  10.0 )
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
    let opts = info (helper <*> options)
               ( fullDesc
                 <> progDesc "Render a scene using ray tracing"
                 <> header "When is this text displayed?"
               )
    execParser opts >>= hCast
