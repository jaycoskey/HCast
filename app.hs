module App where

-- ===== AppConfig =====
data AppConfig = AppConfig
    { maxColorVal :: Int
    }

-- ===== AppOptions =====
data AppOptions = AppOptions
    { appOFile  :: String
    , appWidth  :: Int
    , appHeight :: Int
    -- , quiet :: Bool
    }

