module Light (Light) where

import Color
import Geometry

-- type Fov = Double

data Light = AmbientLight Color
           | Spotlight    Color Pos3f Vec3f
           | PointLight   Color Pos3f

