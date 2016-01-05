module Camera where

import Geometry

-- TODO: Orthographic, Perspective, Fisheye cameras, etc.
data Camera = Camera
    { posn :: Pos3d
    , dirn :: Vec3d -- lookAt :: Pos3d
    , up   :: Vec3d
    , hFov :: Double
    }
    deriving (Show)