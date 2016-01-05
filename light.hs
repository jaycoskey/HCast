module Light where

import Color
import Geometry
-- import Text.Show

-- type Fov = Double

data Light = AmbientLight Color
           | Spotlight    Color Pos3d Vec3d
           | PointLight   Color Pos3d
           deriving (Show)

-- instance Show Light where
--     show (AmbientLight c)     = "AmbientLight: color=" ++ show c
--     show (Spotlight    c p v) = "Spotlight: color="    ++ show c
--                                       ++ "; pos="      ++ show p
--                                       ++ "; dir="      ++ show v
--     show (Spotlight    c p v) = "PointLight: color="   ++ show c
--                                       ++ "; pos="     ++ show p