-- module SceneFile where

{-# LANGUAGE Arrows #-} -- NoMonomorphismRestriction

import Text.Parse
import Text.XML.HXT.Core

import Camera
import Color
import Light
import Shape
 
data Scene = Scene
  { backgroundColor :: Color
  , maxDepth        :: Int
  , objects         :: [Object]
  , cameras         :: [Camera]
  , lights          :: [Light]
  }
  deriving (Show)

data CsgType = CsgIntersection
             | CsgUnion
             | CsgDiff
             | CsgSymDiff
             deriving (Show)

type Name = String

data ObjectBase = ObjectBase
  { name   :: String
    ,color  :: Color
    -- TODO: xforms :: [Transform]  -- Scale, Translate, and Rotate
  }
  deriving (Show)

data Object = RawObject ObjectBase Shape
            | CsgObject ObjectBase CsgType (Maybe Object) (Maybe Object)
            deriving (Show)

parseXML file = readDocument [ withValidate yes, withRemoveWS yes ]
                             file

atTag tag = deep (isElem >>> hasName tag)

dummyColor = black

-- Semantic constraints on HCast Scene files:
--   * The "color" element can have attributes r,g,b, or name, but not both.
--       - The valid color names are available in a data file, together with their RGB values.
--   * The "rotate" element has two restrictions:
--       - It can have either the attribute "radians", or the attribute "degrees", but not both.
--       - It can have either the attribute "axis" or the attributes "x","y","z", but not both.

getScene = atTag "scene" >>>
  proc scene -> do
    -- sceneName       <- getAttrValue "name"                 -< scene
    -- sceneRef        <- getAttrValue "ref"                  -< scene
    -- backgroundColorRef <- getAttrValue "backgroundColor"   -< scene
    maxDepth           <- getAttrValue "maxDepth"             -< scene
    sceneObjects       <- listA getObject <<< atTag "object"  -< scene
    sceneCameras       <- listA getCamera <<< atTag "camera"  -< scene
    sceneLights        <- listA getLight  <<< atTag "light"   -< scene
    returnA -< Scene { -- name = sceneName
                     -- ,
                     backgroundColor = dummyColor
                     , maxDepth      = (read maxDepth) :: Int
                     , objects       = sceneObjects
                     , cameras       = sceneCameras
                     , lights        = sceneLights                     
                     }

getObject = atTag "object" >>>
  proc object -> do
    objName      <- getAttrValue "name"    -< object
    objRef       <- getAttrValue "ref"     -< object

    intersection <- atTag "intersection"   -< object
    union        <- atTag "union"          -< object
    diff         <- atTag "diff"           -< object
    symDiff      <- atTag "symDiff"        -< object

    shape        <- atTag "shape"          -< object
    shapeName    <- getAttrValue "name"    -< shape
    shapeRef     <- getAttrValue "ref"     -< shape
    shapeType    <- getAttrValue "type"    -< shape
    centerX      <- getAttrValue "centerX" -< shape
    centerY      <- getAttrValue "centerY" -< shape
    centerZ      <- getAttrValue "centerZ" -< shape
    shapeRadius  <- getAttrValue "radius"  -< shape

    color        <- atTag "color"          -< object
    colorR       <- getAttrValue "r"       -< color
    colorG       <- getAttrValue "g"       -< color
    colorB       <- getAttrValue "b"       -< color
    -- xformStack   <- atTag "TEAM"        -< object
    returnA -< -- if intersection then Object ObjectBase { name = objName
               --                                        , color = (colorR, colorG, colorB)
               --                                        }
               --                                        Nothing Nothing
               -- else if union   then Object ObjectBase { name = objName
               --                                        , color = (colorR, colorG, colorB)
               --                                        }
               --                                        Nothing Nothing
               -- else if diff    then Object ObjectBase { name = objName
               --                                        , color = (colorR, colorG, colorB)
               --                                        }
               --                                        Nothing Nothing
               -- else if symDiff then Object ObjectBase { name = objName
               --                                        , color = (colorR, colorG, colorB)
               --                                        }
               --                                        Nothing Nothing
               -- else
                    RawObject (
                                ObjectBase
                                  { name  = objName
                                  , color = ((read colorR) :: Double, (read colorG) :: Double, (read colorB) :: Double)
                                  }
                              )
                              (
                                Sphere
                                  ( (read centerX) :: Double
                                  , (read centerY) :: Double
                                  , (read centerZ) :: Double
                                  )
                                  ((read shapeRadius) :: Double)
                              )

getCamera = atTag "camera" >>>
  proc camera -> do
    camName      <- getAttrValue "name"    -< camera
    camRef       <- getAttrValue "ref"     -< camera
    camType      <- getAttrValue "type"    -< camera
    camEnabled   <- getAttrValue "enabled" -< camera

    position     <- atTag "position"       -< camera
    camPosX      <- getAttrValue "x"       -< position
    camPosY      <- getAttrValue "y"       -< position
    camPosZ      <- getAttrValue "z"       -< position

    direction    <- atTag "direction"      -< camera
    camDirX      <- getAttrValue "x"       -< direction
    camDirY      <- getAttrValue "y"       -< direction
    camDirZ      <- getAttrValue "z"       -< direction

    target       <- atTag "target"         -< camera
    camTargetX   <- getAttrValue "x"       -< target
    camTargetY   <- getAttrValue "y"       -< target
    camTargetZ   <- getAttrValue "z"       -< target

    up           <- atTag "up"             -< camera
    camUpX       <- getAttrValue "x"       -< up
    camUpY       <- getAttrValue "y"       -< up
    camUpZ       <- getAttrValue "z"       -< up

    hFov         <- atTag "hFov"           -< camera
    camHFov      <- getAttrValue "value"   -< hFov  
    returnA -< Camera { -- name      = camName
                      -- , camType   = camType
                      -- , enabled   = camEnabled
                      -- ,
                        posn = (read camPosX, read camPosY, read camPosZ) :: (Double, Double, Double)
                      , dirn = (read camDirX, read camDirY, read camDirZ) :: (Double, Double, Double)
                      , up   = (read camUpX,  read camUpY,  read camUpZ)  :: (Double, Double, Double)
                      , hFov = (read camHFov) :: Double
                      }

getLight = atTag "light" >>>
  proc light -> do
    lightName    <- getAttrValue "name"    -< light
    lightType    <- getAttrValue "type"    -< light
    color        <- atTag "color"          -< light
    colorR       <- getAttrValue "r"       -< color
    colorG       <- getAttrValue "g"       -< color
    colorB       <- getAttrValue "b"       -< color

    position     <- atTag "position"       -< light
    lightPosX    <- getAttrValue "x"       -< position
    lightPosY    <- getAttrValue "y"       -< position
    lightPosZ    <- getAttrValue "z"       -< position

    direction    <- atTag "direction"      -< light
    lightDirX    <- getAttrValue "x"       -< direction
    lightDirY    <- getAttrValue "y"       -< direction
    lightDirZ    <- getAttrValue "z"       -< direction
    returnA -< -- if lightType == "spotlight"
               -- then
               Spotlight ((read colorR)    :: Double, (read colorG)    :: Double, (read colorB)    :: Double)
                         ((read lightPosX) :: Double, (read lightPosY) :: Double, (read lightPosZ) :: Double)
                         ((read lightDirX) :: Double, (read lightDirY) :: Double, (read lightDirZ) :: Double)
 
main = do
  scene <- runX (parseXML "sceneFileTest.xml" >>> getScene)
  print scene