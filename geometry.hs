module Geometry where

import Numeric
import Test.HUnit

type Pos3d = (Double, Double, Double)
type Vec3d = (Double, Double, Double)

xUnit = (1.0, 0.0, 0.0)
yUnit = (0.0, 1.0, 0.0)
zUnit = (0.0, 0.0, 1.0)
zero3 = (0.0, 0.0, 0.0)

xU = xUnit
yU = yUnit
zU = zUnit
z3 = zero3

eqVec3Eps :: Vec3d -> Vec3d -> Double -> Bool
eqVec3Eps v1 v2 epsilon = norm2 (v1 <-> v2) < epsilon

degrees = pi / 180.0

debugShowVec3 (a, b, c) = "(" 
                          ++ (showFFloat (Just 2) a "") ++ ", "
                          ++ (showFFloat (Just 2) b "") ++ ", "
                          ++ (showFFloat (Just 2) c "") 
                          ++ ")"

data Ray = Ray { orig :: Pos3d
               , dir  :: Vec3d
               } deriving Show

-- TODO! Set fixity
(.^) :: Double -> Vec3d -> Vec3d
(.^) d (x, y, z) = (d*x, d*y, d*z)

dot :: Vec3d -> Vec3d -> Double
dot (x1, y1, z1) (x2, y2, z2) = (x1*x2 + y1*y2 + z1*z2)

norm2 :: Vec3d -> Double
norm2 vec = vec `dot` vec

norm :: Vec3d -> Double
norm vec = sqrt $ norm2 vec

normalize :: Vec3d -> Vec3d
normalize vec = (1 / (norm vec)) .^ vec

-- TODO! Set fixity
(<+>) :: Pos3d -> Vec3d -> Vec3d
(<+>) (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

(<->) :: Pos3d -> Vec3d -> Vec3d
(<->) (x1, y1, z1) (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)

-- Borrow operator from Data.Vec3.
(><) :: Vec3d -> Vec3d -> Vec3d
(><) (x1, y1, z1) (x2, y2, z2)
    = ( y1 * z2 - y2 * z1
      , x2 * z1 - x1 * z2
      , x1 * y2 - x2 * y1)

rayAtTime :: Ray -> Double -> Pos3d
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

eps = 0.01

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

