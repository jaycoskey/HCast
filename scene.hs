module Scene where

import Light
import Object

-- TODO: Convert to scene graph, as in Coin3D and FEI's OpenInventor.
-- TODO: Efficiency: Groups w/ bounding spheres, kd-trees, etc.
data Scene = Scene {
    objects :: [Object]
    ,lights :: [Light]
    }

