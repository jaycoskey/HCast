module Screen where

import Color
import Object

-- Note: For now, this is the prospective place to add super-sampling results.
-- When added, there will be a reduction step between Screen and output Image.
data Screen = Screen { width :: Int
                     , height :: Int
                     , colors :: [Color]  -- TODO: Use Array representation
                     }

