-- For future performance improvements:
-- {-# LANGUAGE BangPatterns #-}

-- Compiler flags to keep in mind:
--   -threaded -O2 -funbox-strict-fields -fexcess-precision
--   -funfolding-keeness-factor=10 -fvia-C -optc-O3
--   -optc-msse3 -optc-ffast-math

-- import System.IO

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
  { posn :: Pos3
  , dirn :: Vec3  -- lookAt :: Pos3
  , up   :: Vec3
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
type Pos3 = (Double, Double, Double)
type Vec3 = (Double, Double, Double)

xUnit = (1.0, 0.0, 0.0)
yUnit = (0.0, 1.0, 0.0)
zUnit = (0.0, 0.0, 1.0)
zero3 = (0.0, 0.0, 0.0)

xU = xUnit
yU = yUnit
zU = zUnit
z3 = zero3

eqVec3Eps :: Vec3 -> Vec3 -> Double -> Bool
eqVec3Eps v1 v2 epsilon = norm2 (v1 <-> v2) < epsilon

degrees = pi / 180.0

debugShowVec3 (a, b, c) = "(" 
                       ++ (showFFloat (Just 2) a "") ++ ", "
                       ++ (showFFloat (Just 2) b "") ++ ", "
                       ++ (showFFloat (Just 2) c "") 
                       ++ ")"

data Ray = Ray
  { orig :: Pos3
  , dir  :: Vec3
  } deriving Show

-- TODO! Set fixity
(.^) :: Double -> Vec3 -> Vec3
(.^) d (x, y, z) = (d*x, d*y, d*z)

dot :: Vec3 -> Vec3 -> Double
dot (x1, y1, z1) (x2, y2, z2) = (x1*x2 + y1*y2 + z1*z2)

norm2 :: Vec3 -> Double
norm2 vec = vec `dot` vec

norm :: Vec3 -> Double
norm vec = sqrt $ norm2 vec

normalize :: Vec3 -> Vec3
normalize vec = (1 / (norm vec)) .^ vec

-- TODO! Set fixity
(<+>) :: Pos3 -> Vec3 -> Vec3
(<+>) (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

(<->) :: Pos3 -> Vec3 -> Vec3
(<->) (x1, y1, z1) (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)

-- Borrow operator from Data.Vec3.
(><) :: Vec3 -> Vec3 -> Vec3
(><) (x1, y1, z1) (x2, y2, z2)
    = ( y1 * z2 - y2 * z1
      , x2 * z1 - x1 * z2
      , x1 * y2 - x2 * y1)

rayAtTime :: Ray -> Double -> Pos3
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
              ++ "; nAxis="  ++ (debugShowVec3 nAxis)
              ++ "; dp="     ++ (debugShow2 dp)
              ++ "; cp="     ++ (debugShowVec3 cp)
              ++ "; v="      ++ (debugShowVec3 v)
              ++ "; result=" ++ (debugShowVec3 result)
              ++ "\n"

eps = 0.0001

-- TODO: Use Test.HUnit.Approx
testCrossProdX = TestCase $ assertBool "testCrossProdX" $ eqVec3Eps (yU >< zU) xU eps
testCrossProdY = TestCase $ assertBool "testCrossProdY" $ eqVec3Eps (zU >< xU) yU eps
testCrossProdZ = TestCase $ assertBool "testCrossProdZ" $ eqVec3Eps (xU >< yU) zU eps
testCrossProdList = TestList [ TestLabel "testCrossProdX" testCrossProdX
                             , TestLabel "testCrossProdY" testCrossProdY
                             , TestLabel "testCrossProdZ" testCrossProdZ
                             ]

testRotateX = TestCase $ assertBool "testRotateX" $ eqVec3Eps (yU >< zU) xU eps
testRotateY = TestCase $ assertBool "testRotateY" $ eqVec3Eps (zU >< xU) yU eps
testRotateZ = TestCase $ assertBool "testRotateZ" $ eqVec3Eps (xU >< yU) zU eps
testRotateList = TestList [ TestLabel "testRotateX" testRotateX
                          , TestLabel "testRotateY" testRotateY
                          , TestLabel "testRotateZ" testRotateZ
                          ]

debugShowCrossProduct v1 v2 = testResultStr
  where
    result    = v1 >< v2
    v1Str     = debugShowVec3 v1
    v2Str     = debugShowVec3 v2
    resultStr = debugShowVec3 result
    testResultStr = "Cross product of " ++ (debugShowVec3 v1)
                    ++ " with "         ++ (debugShowVec3 v2)
                    ++ " = "            ++ (debugShowVec3 result)

debugShowCrossProducts = testResultStr
  where
    result1 = debugShowCrossProduct yUnit zUnit  -- Result = xUnit
    result2 = debugShowCrossProduct zUnit xUnit  -- Result = yUnit
    result3 = debugShowCrossProduct xUnit yUnit  -- Result = zUnit
    testResultStr = result1 ++ result2 ++ result3

debugShowRotation axis deg v = testResultStr
  where
    angle     = deg * degrees
    axisStr   = debugShowVec3 axis
    vStr      = debugShowVec3 v
    result    = rotateAroundAxisByAngle axis angle v
    resultStr = debugShowVec3 result
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
data Light = AmbientLight Color
           | Spotlight    Color Pos3 Vec3
           | PointLight   Color Pos3

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
type Transformation = DummyType  -- TODO: Implement
type Material = Color            -- TODO: Implement
type Intersection = Color        -- TODO: Implement (HACK: For now, identify intersection w/ pixel color there)

data Object = Object Shape Material Transformation Intersection

-- TODO!  Remove hack
mkShape :: Object -> Shape
mkShape (Object shape _ _ _) = shape

mkObject :: Shape -> Object
mkObject shape = Object shape red 1 red

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
type Normal = Vec3
type Axis   = Vec3
type Mesh   = Int  -- TODO: Implement

data Shape = Sphere        Pos3 Radius
           | Plane       Pos3 Normal

           | Cone        Pos3 Radius Axis
           | Cylinder    Pos3 Radius Axis
           | PolygonMesh Pos3 Mesh  -- For now, Solid object only
           | Torus       Pos3 Float Float  -- major radius, minor radius

           | Tetrahedron  Pos3  -- TODO: Document default orientation
           | Cube         Pos3  -- TODO: Document default orientation
           | Octahedron   Pos3  -- TODO: Document default orientation
           | Dodecahedron Pos3  -- TODO: Document default orientation
           | Icosahedron  Pos3  -- TODO: Document default orientation

           -- TODO: Move CSG feature into Scene graph
           | CsgDiff     Shape Shape
           | CsgSymDiff  Shape Shape
           | CsgUnion    Shape Shape

isBounded :: Shape -> Bool
isBounded (Plane _ _) = False
isBounded _ = True

isSolid :: Shape -> Bool
isSolid (Plane _ _) = False
isSolid _ = True

intersect :: Shape -> Ray -> Maybe Intersection
intersect (Sphere pos rad) (Ray { orig=orig, dir=dir }) =
    case (maybeIntersectTime) of
        Just root -> if root > 0 then Just red else Nothing
        Nothing   -> Nothing
    where
        delta = orig <-> pos
        a = dir `dot` dir
        b = 2 * (dir `dot` delta)
        c = (delta `dot` delta) - rad*rad
        maybeIntersectTime = minQuadraticRoot a b c

intersect (Plane pos norm) (Ray { orig=orig, dir=dir }) =
    -- TODO: Complete
    Nothing

intersect _ _ = Nothing

-- ===== HCastrMain =====
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

getColor :: Scene -> Camera -> Int -> Int -> Int -> Int -> Color
getColor
  (Scene  { objects=objs, lights=lux })
  (Camera { posn=camPos, dirn=camDir, up=camUp, hFov=camHFov })
  w h i j
    = case maybeIntersection of  -- TODO: Replace w/ actual impl.
          Just color -> color
          Nothing    -> black
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
          headObj = head objs  -- HACK
          targetShape = mkShape headObj
          maybeIntersection = intersect targetShape ray

scene = Scene
  { objects = [ mkObject $ Sphere (0, 0, 2) 3.0
              , mkObject $ Plane  (0, 0, 0) (0, 0, 1)
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
