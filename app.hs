module App where

import Color

-- ===== AppConfig =====
data AppConfig = AppConfig
    { backgroundColor :: Color
    , maxColorVal :: Int
    , maxDepth    :: Int
    }

appConfig = AppConfig { backgroundColor = black
                      , maxColorVal = 255
                      , maxDepth = 0
                      }

-- ===== AppOptions =====
data AppOptions = AppOptions
    { appOFile  :: String
    , appWidth  :: Int
    , appHeight :: Int
    -- , quiet :: Bool
    }