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
                    , point    :: Pos3d
                    , normal   :: Vec3d
                    , color    :: Color
                    , material :: Material
                    }

cmpDistance :: Intersection -> Intersection -> Ordering
cmpDistance int1@Intersection { ray=_
                              , distance=dist1
                              , point=_
                              , normal=_
                              , color=_
                              , material=_
                              }
            int2@Intersection{ ray=_
                              , distance=dist2
                              , point=_
                              , normal=_
                              , color=_
                              , material=_
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
    case (intersectTimes) of
        [time] -> Just Intersection
                      { ray      = ray
                      , distance = time * norm(dir)
                      , point    = intersectionPoint
                      , normal   = normalize(intersectionPoint <-> center)
                      , color    = objColor
                      , material = objMaterial
                      }
                    where
                      intersectionPoint  = center <+> (time .^ dir)
        [] -> Nothing
    where
        delta = orig <-> center
        a = dir `dot` dir
        b = 2 * (dir `dot` delta)
        c = (delta `dot` delta) - rad*rad
        roots = quadraticRoots a b c
        posRoots = filter (> 0) roots
        intersectTimes = if (length posRoots <= 1)
                         then posRoots
                         else [min (posRoots!!0) (posRoots!!1)]

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