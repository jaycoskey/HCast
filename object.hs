module Object where

import Color
import Geometry
import Math
import Shape

type DummyType = Int             -- TODO: Remove when no longer used
type Material = DummyType        -- TODO: Implement
type Transformation = DummyType  -- TODO: Implement
dummyVal = 1

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
            int2@Intersection{ ray=_
                              , distance=dist2
                              , point=_
                              , normal=_
                              , color=_
                              }
    | dist1 < dist2 = LT
    | otherwise     = GT

data Object = Object Shape Color Material Transformation 

mkObject :: Shape -> Color -> Object
mkObject shape color = Object shape color dummyVal dummyVal

intersect :: Object -> Ray -> Maybe Intersection
intersect
  obj@( Object
          shape@(Sphere center rad)
          objColor
          objMaterial
          objXform
      )
  ray@(Ray { orig=orig, dir=dir })
  =
    case (maybeIntersectTime) of
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

intersect obj@(Object
                 (Plane pos norm)
                 objColor
                 objMaterial
                 objXform
              )
          (Ray { orig=orig, dir=dir }) =
    -- TODO: Complete
    Nothing

intersect _ _ = Nothing

