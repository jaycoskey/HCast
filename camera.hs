module Camera where

import Geometry

-- TODO: Orthographic, Perspective, Fisheye cameras, etc.
data Camera = Camera
    { posn :: Pos3f
    , dirn :: Vec3f -- lookAt :: Pos3f
    , up   :: Vec3f
    , hFov :: Double
    }

