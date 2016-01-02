-- {-# LANGUAGE BangPatterns #-}  -- For future performance improvements

import Data.Function  -- For the definition of `on`
import Options.Applicative

-- ===== AppConfig =====
data AppConfig = AppConfig
  { maxColorVal :: Int
  }

-- ===== AppOptions =====
data AppOptions = AppOptions
  { ofile :: String
  , quiet :: Bool
  }

-- ===== Camera =====
data Camera = Camera
  { posn :: Pos3
  , dirn :: Vec3 -- lookAt :: Pos3
  , up   :: Vec3
  , fov  :: Double
  }

-- ===== Color =====
-- TODO: Netpbm P3 for prototype.  Move to other file format(s) later.
-- TODO: Compare performance of different color representations.
type Color = (Double, Double, Double)
type Pixel = (Int, Int, Int)

toPixel (r, g, b) = (round(255*r), round(255*g), round(255*b))
showPixel (r, g, b) = (show r ++ " " ++ show g ++ " " ++ show b)

-- ===== Geometry =====
type Pos3 = (Double, Double, Double)
type Vec3 = (Double, Double, Double)

data Ray = Ray
  { orig :: Pos3
  , dir :: Vec3
  } deriving Show

-- TODO! Set fixity
(*:) :: Double -> Vec3 -> Vec3
(*:) d (v1, v2, v3) = (d*v1, d*v2, d*v3)

-- TODO! Set fixity
(+:) :: Pos3 -> Vec3 -> Vec3
(+:) (a1, b1, c1) (a2, b2, c2) = (a1+b1, b1+b2, c1+c2)

rayAtTime :: Ray -> Double -> Pos3
rayAtTime (Ray orig dir) t = orig +: (t *: dir)

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
type Transformation = Int  -- TODO: Implement
type Material = Int  -- TODO: Implement
type Intersection = Int  -- TODO: Implement

data Object = Object Shape Material Transformation Intersection

-- ===== Scene =====
-- TODO: Convert to scene graph, as in Coin3D and FEI's OpenInventor.
-- TODO: Efficiency: Groups w/ bounding spheres, kd-trees, etc.
data Scene = Scene
  { objects :: [Object]
  , lights :: [Light]
  }

-- ===== Screen =====
-- Note: For now, this is the prospective place to add super-sampling results.
-- When added, there will be a reduction step between Screen and output Image.
data Screen = Screen
  { width :: Int
  , height :: Int
  , colors :: [Color]  -- TODO: Use Array representation
  }

-- ===== Shape =====
type Radius = Double
type Normal = Vec3
type Axis = Vec3
type Mesh = Int  -- TODO: Implement

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
intersect (Sphere _ rad) (Ray { orig=orig, dir=dir }) =
    -- TODO: Complete
    Nothing

intersect _ _ = Nothing

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
