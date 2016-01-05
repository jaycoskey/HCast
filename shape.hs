module Shape where

import Geometry

type Radius = Double
type Normal = Vec3d
type Axis   = Vec3d
type Mesh   = Int  -- TODO: Implement

data Shape = Sphere        Pos3d Radius
             | Plane       Pos3d Normal

             | Cone        Pos3d Radius Axis
             | Cylinder    Pos3d Radius Axis
             | PolygonMesh Pos3d Mesh  -- For now, Solid object only
             | Torus       Pos3d Double Double -- major radius, minor radius

             | Tetrahedron  Pos3d  -- TODO: Document default orientation
             | Cube         Pos3d  -- TODO: Document default orientation
             | Octahedron   Pos3d  -- TODO: Document default orientation
             | Dodecahedron Pos3d  -- TODO: Document default orientation
             | Icosahedron  Pos3d  -- TODO: Document default orientation

             -- TODO: Move CSG feature into Scene graph
             | CsgIntersection Shape Shape
             | CsgUnion        Shape Shape
             | CsgDiff         Shape Shape
             | CsgSymDiff      Shape Shape
             deriving (Show)

isBounded :: Shape -> Bool
isBounded (Plane _ _) = False
isBounded _ = True

isSolid :: Shape -> Bool
isSolid (Plane _ _) = False
isSolid _ = True