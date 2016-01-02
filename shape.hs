module Shape where

import Geometry

type Radius = Double
type Normal = Vec3f
type Axis   = Vec3f
type Mesh   = Int  -- TODO: Implement

data Shape = Sphere        Pos3f Radius
             | Plane       Pos3f Normal

             | Cone        Pos3f Radius Axis
             | Cylinder    Pos3f Radius Axis
             | PolygonMesh Pos3f Mesh  -- For now, Solid object only
             | Torus       Pos3f Double Double -- major radius, minor radius

             | Tetrahedron  Pos3f  -- TODO: Document default orientation
             | Cube         Pos3f  -- TODO: Document default orientation
             | Octahedron   Pos3f  -- TODO: Document default orientation
             | Dodecahedron Pos3f  -- TODO: Document default orientation
             | Icosahedron  Pos3f  -- TODO: Document default orientation

             -- TODO: Move CSG feature into Scene graph
             | CsgDiff     Shape Shape
             | CsgSymDiff  Shape Shape
             | CsgUnion    Shape Shape

isBounded :: Shape -> Bool
isBounded (Plane _ _) = False
isBounded _ = True

isSolid :: Shape -> Bool
isSolid (Plane _ _) = False
isSolid _ = True

