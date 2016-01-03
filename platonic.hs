module Platonic where

import Data.List (nub, nubBy)
import Debug.Trace (trace)
import Numeric (showFFloat)
import Test.HUnit (assertEqual, Test(..)) -- TestCase), TestLabel, TestList)

-- =============================
-- ===== Numeric constants =====
-- =============================
phi, phiInv, phiInv2, sqrt2Inv :: Double
phi      = ((sqrt 5.0) + 1.0) / 2.0
phiInv   = ((sqrt 5.0) - 1.0) / 2.0
phiInv2  = phiInv * phiInv 
sqrt2Inv = 1.0 / (sqrt 2.0)

signs    = [-1.0, 1.0]

-- =============================
-- ===== Numeric functions =====
-- =============================
epsilon = 0.0001
sqr x = x * x
isZero x = abs x < epsilon
areEqual x y = isZero (x - y)
half n = (fromIntegral n) / 2

-- ==========================
-- ===== Misc functions =====
-- ==========================
showLength :: [a] -> String
showLength xs = show $ length xs

-- ========================
-- ===== Vector types =====
-- ========================

-- ===== VECTORS =====
type Vec3f = (Double, Double, Double)

scaleVec3 s v@(x,y,z) = (s*x, s*y, s*z)

norm2, norm :: Vec3f -> Double
norm2 (x, y, z) = x*x + y*y + z*z
norm vec = sqrt $ norm2 vec

(<->) :: Vec3f -> Vec3f -> Vec3f 
(<->) (x1, y1, z1) (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)

vec3fDist :: Vec3f -> Vec3f -> Double
vec3fDist v1 v2 = norm $ v1 <-> v2

showFloat x = showFFloat (Just 4) x ""

showVec3f (x,y,z) = "("      ++ (showFloat x)
                     ++ ", " ++ (showFloat y)
                     ++ ", " ++ (showFloat z)
                     ++ ")"

indent str = "\t" ++ str

printLabeledVec3fs :: String -> [VIndexedVec3f] -> IO()
printLabeledVec3fs label vs = do
    putStrLn $ label ++ " (" ++ (showLength vs) ++ " verts): "
    mapM_ putStrLn $ map (indent . showVIndexedVec3f) vs

cyclicPerms :: Vec3f -> [Vec3f]
cyclicPerms (x,y,z) = [(x,y,z), (y,z,x), (z,x,y)]

allSignsX, allSignsY, allSignsXY, allSignsXYZ :: Vec3f -> [Vec3f]
allSignsX  (x,y,z) = [(sx*x,   y,   z) | sx <- signs ]
allSignsY  (x,y,z) = [(   x,sy*y,   z) |              sy <- signs]
allSignsXY (x,y,z) = [(sx*x,sy*y,   z) | sx <- signs, sy <- signs]
allSignsYZ (x,y,z) = [(   x,sy*y,sz*z) |              sy <- signs, sz <- signs]
allSignsXZ (x,y,z) = [(sx*x,   y,sz*z) | sx <- signs,              sz <- signs]
allSignsXYZ(x,y,z) = [(sx*x,sy*y,sz*z) | sx <- signs, sy <- signs, sz <- signs]

-- ===== LABELED VECTORS =====
type VIndexedVec3f = (VIndex, Vec3f)

applyVIndex :: [Vec3f] -> [VIndexedVec3f]
applyVIndex vecs = zip [0..] vecs

showVIndexedVec3f (id, v) = "#" ++ show(id) ++ ": " ++ (showVec3f v)
printVIndexedVec3s vIndVecs = do
    mapM_ putStrLn $ map showVIndexedVec3f vIndVecs

-- ===== EDGES =====
type Edge = (VIndexedVec3f, VIndexedVec3f, Double)

completeGraph :: [VIndexedVec3f] -> [Edge]
completeGraph vIndVecs = [((vInd1, vec1), (vInd2, vec2), vec3fDist vec1 vec2)
                   | (vInd1, vec1) <- vIndVecs
                   , (vInd2, vec2) <- vIndVecs
                   , vInd1 /= vInd2
               ]
showEdge (vIndVec1, vIndVec2, dist) =
    "("     ++ showVIndexedVec3f vIndVec1
    ++ ", " ++ showVIndexedVec3f vIndVec2
    ++ ", " ++ (showFloat dist)
    ++ ")"

printEdges edges = do
    mapM_ putStrLn $ map showEdge edges

edgeLength :: Edge -> Double
edgeLength e@(lv1, lv2, d) = d

minEdgeLength :: [Edge] -> Double
minEdgeLength es = minimum $ map edgeLength es
 
adjEdges :: [VIndexedVec3f] -> [Edge]
adjEdges vIndVecs = filter hasMinLength edges
  where edges = completeGraph vIndVecs
        minLength = minEdgeLength edges
        hasMinLength = \e@(lv1, lv2, d) -> areEqual d minLength
  
printShortestEdges edges = do
    let minLength    = minEdgeLength edges
    let hasMinLength = \e@(lv1, lv2, d) -> areEqual d minLength
    printEdges $ filter hasMinLength edges

-- ===== PATHS =====
type VIndex = Int
type Path = [VIndex]  -- List of Edge vIndexs

adjVIndexs :: [Edge] -> VIndex -> [VIndex]
adjVIndexs edges orig = [y | ((x,_), (y,_), _) <- edges, x == orig]

areConnected :: [Edge] -> VIndex -> VIndex -> Bool
areConnected edges vIndex1 vIndex2 =
    length [(vInd1, vInd2)
               | ((vInd1,_), (vInd2,_), _) <- edges
                 , vInd1 == vIndex1
                 , vInd2 == vIndex2
           ] > 0

pathsOfLengthFromBeginning :: [Edge] -> Int -> VIndex -> [Path]
pathsOfLengthFromBeginning edges tgtLength begin = 
    -- pathsOfLengthFromBeginningImpl :: [Edge] -> VIndex -> [Path]
    pathsOfLengthFromBeginningImpl edges (tgtLength - 1) [[begin]]
      where
        pathsOfLengthFromBeginningImpl edges addLength accPaths
          | addLength <= 0 = accPaths
          | otherwise      =
              concat [pathsOfLengthFromBeginningImpl
                       edges
                       (addLength - 1)
                       [nextVIndex : accPath]
                         | accPath <- accPaths 
                         , nextVIndex <- adjVIndexs edges $ head accPath
                         , not $ elem nextVIndex accPath
                     ]

pathsOfLength :: [Edge] -> Int -> [Path]
pathsOfLength edges tgtLength =
    concat [pathsOfLengthFromBeginning edges tgtLength beginVIndex 
               | beginVIndex <- vertVIndexs
           ]
      where vertVIndexs = nub [vInd1 | ((vInd1, _), _, _) <- edges]

cyclesOfLength :: [Edge] -> Int -> [Path]
cyclesOfLength edges tgtLength = uniqueCycles
  where
    paths = pathsOfLength edges tgtLength
    isPathCyclic = \path -> areConnected edges (head path) (last path)
    cycles = filter isPathCyclic paths
    rotateList (x : xs) = xs ++ [x]
    rotateList [] = []
    allRotations list = take (length list) $ iterate rotateList list
    allRotationsAndInversions list =
        (allRotations list) ++ (allRotations $ reverse list)
    areCyclesEqual cyc1 cyc2 = elem cyc1 $ allRotationsAndInversions cyc2
    uniqueCycles = nubBy areCyclesEqual cycles 

printPathList :: [Path] -> IO ()
printPathList paths = mapM_ (putStrLn . indent . show) paths
      
-- ======================================
-- ===== Individual Platonic solids =====
-- ======================================
type Face = Path

vIndexVec3fNorm :: VIndexedVec3f -> Double
vIndexVec3fNorm v@(vIndex, vec) = norm vec

data Polyhedron = Polyhedron { verts :: [VIndexedVec3f]
                             , edges :: [Edge]
                             , faces :: [Face]
                             } deriving (Show)

showPolyhedron :: Polyhedron -> String
showPolyhedron Polyhedron {verts=v, edges=e2, faces=f} =
    "(#v=" ++ (show $ length v)
           ++ ", 2*#e=" ++ (show $ length e2)
           ++ ", #f="   ++ (show $ length f)
           ++ ")"

tetraVerts, cubeVerts, octaVerts, dodecaVerts, icosaVerts :: [VIndexedVec3f]
tetraEdges, cubeEdges, octaEdges, dodecaEdges, icosaEdges :: [Edge]
tetraFaces, cubeFaces, octaFaces, dodecaFaces, icosaFaces :: [Face]

tetraBase1  = (1.0, 0.0, -1.0 * sqrt2Inv)
tetraBase2  = (0.0, 1.0,        sqrt2Inv)
tetraScale  = 1.0 / (sqrt 1.5)
tetraVerts  = applyVIndex $ map scaleTetra verts
                where scaleTetra = scaleVec3 tetraScale
                      verts      = allSignsX tetraBase1
                                   ++ allSignsY tetraBase2
tetraEdges  = adjEdges tetraVerts
tetraFaces  = cyclesOfLength tetraEdges 3
tetraInfo   = (length tetraVerts, length tetraEdges, length tetraFaces)
tetrahedron = Polyhedron { verts = tetraVerts
                         , edges = tetraEdges
                         , faces = tetraFaces
                         }

cubeBase    = (1.0, 1.0, 1.0)
cubeScale   = 1.0 / (sqrt 3.0)
cubeVerts   = applyVIndex $ map scaleCube verts
                where scaleCube = scaleVec3 cubeScale
                      verts = allSignsXYZ cubeBase
cubeEdges   = adjEdges cubeVerts
cubeFaces   = cyclesOfLength cubeEdges 4
cubeInfo    = (length cubeVerts, length cubeEdges, length cubeFaces)
cube        = Polyhedron { verts = cubeVerts
                         , edges = cubeEdges
                         , faces = cubeFaces
                         }

octaBase    = (1.0, 0.0, 0.0)
octaScale   = 1.0 -- No need to scale
octaVerts   = applyVIndex $ concat $ map cyclicPerms $ allSignsX octaBase
octaEdges   = adjEdges octaVerts
octaFaces   = cyclesOfLength octaEdges 3
octaInfo    = (length octaVerts, length octaEdges, length octaFaces)
octahedron  = Polyhedron { verts = octaVerts
                         , edges = octaEdges
                         , faces = octaFaces
                         }

dodecaBase0 = (1.0,           1.0,           1.0)
dodecaBase1 = (0.0,           phi,           1.0 - phiInv2)
dodecaBase2 = (phi,           1.0 - phiInv2, 0.0)
dodecaBase3 = (1.0 - phiInv2, 0.0,           phi)
dodecaScale = 1.0 / (sqrt $ (sqr phi) + (sqr $ 1 - phiInv2))
dodecaVerts = applyVIndex $ map scaleDodeca verts
                where scaleDodeca = scaleVec3 dodecaScale
                      verts = allSignsXYZ   dodecaBase0  -- 8
                              ++ allSignsYZ dodecaBase1  -- 4
                              ++ allSignsXY dodecaBase2  -- 4
                              ++ allSignsXZ dodecaBase3  -- 4
dodecaEdges = adjEdges dodecaVerts
dodecaFaces = cyclesOfLength dodecaEdges 5
dodecaInfo  = (length dodecaVerts, length dodecaEdges, length dodecaFaces)
dodecahedron= Polyhedron { verts = dodecaVerts
                         , edges = dodecaEdges
                         , faces = dodecaFaces
                         }

icosaBase1  = (phi, 1.0, 0.0)
icosaBase2  = (1.0, 0.0, phi)
icosaBase3  = (0.0, phi, 1.0)
icosaScale  = 1.0 / (sqrt 5.0)
icosaVerts  = applyVIndex $ map scaleIcosa verts
                where scaleIcosa = scaleVec3 icosaScale
                      verts = allSignsXY icosaBase1    -- 4
                              ++ allSignsXZ icosaBase2 -- 4
                              ++ allSignsYZ icosaBase3 -- 4
icosaEdges  = adjEdges icosaVerts
icosaFaces  = cyclesOfLength icosaEdges 3
icosaInfo   = (length icosaVerts, length icosaEdges, length icosaFaces)
icosahedron = Polyhedron { verts = icosaVerts
                         , edges = icosaEdges
                         , faces = icosaFaces
                         }

-- =====================================
-- ===== Platonic solids functions =====
-- =====================================
printPlatonicSolids = do
    printLabeledVec3fs "Tetrahedron"  tetraVerts
    printLabeledVec3fs "Cube"         cubeVerts
    printLabeledVec3fs "Octahedron"   octaVerts
    printLabeledVec3fs "Dodecahedron" dodecaVerts
    printLabeledVec3fs "Icosahedron"  icosaVerts

printPlatonicNorms = do
    putStrLn "Tetra:"
    mapM_ putStrLn $ map (show . vIndexVec3fNorm)      tetraVerts
    putStrLn "Cube:"
    mapM_ putStrLn $ map (show . vIndexVec3fNorm)      cubeVerts
    putStrLn "Octa:"
    mapM_ putStrLn $ map (show . vIndexVec3fNorm)      octaVerts
    putStrLn "Dodeca:"
    mapM_ putStrLn $ map (showFloat . vIndexVec3fNorm) dodecaVerts
    putStrLn "Icosa:"
    mapM_ putStrLn $ map (show . vIndexVec3fNorm)      icosaVerts

-- Each bidirectional edge counted twice --- once for (v1,v2); one for (v2,v1).
printPlatonicEdges = do
    putStrLn "Tetra:"
    printEdges tetraEdges
    putStrLn "Cube:"
    printEdges cubeEdges
    putStrLn "Octa:"
    printEdges octaEdges
    putStrLn "Dodeca:"
    printEdges dodecaEdges
    putStrLn "Icosa:"
    printEdges icosaEdges

printPlatonicShortestEdges = do
    putStrLn "All Edges"
    printEdges $ completeGraph cubeVerts
    putStrLn "Shortest Edges"
    printEdges $ adjEdges      cubeVerts

printPlatonicCycles = do
    putStrLn $ "Tetra ("  ++ (showLength tetraFaces)  ++ "): "
    printPathList tetraFaces
    putStrLn $ "Cube ("   ++ (showLength cubeFaces)   ++ "): "
    printPathList cubeFaces
    putStrLn $ "Octa ("   ++ (showLength octaFaces)   ++ "): "
    printPathList octaFaces
    putStrLn $ "Dodeca (" ++ (showLength dodecaFaces) ++ "): "
    printPathList dodecaFaces
    putStrLn $ "Icosa ("  ++ (showLength icosaFaces)  ++ "): "
    printPathList icosaFaces

printPlatonicInfo = do
    putStrLn $ "Tetra "  ++ (showPolyhedron tetrahedron)  ++ ": "
    printPathList tetraFaces
    putStrLn $ "Cube "   ++ (showPolyhedron cube)         ++ ": "
    printPathList cubeFaces
    putStrLn $ "Octa "   ++ (showPolyhedron octahedron)   ++ ": "
    printPathList octaFaces
    putStrLn $ "Dodeca " ++ (showPolyhedron dodecahedron) ++ ": "
    printPathList dodecaFaces
    putStrLn $ "Icosa "  ++ (showPolyhedron icosahedron)  ++ ": "
    printPathList icosaFaces

printPlatonic = do
    printPlatonicSolids
    printPlatonicNorms
    printPlatonicEdges
    printPlatonicShortestEdges
    printPlatonicCycles
    printPlatonicInfo
    printEdges icosaEdges

testVertCountTetra  = TestCase $ assertEqual "testVertCountTetra"  (length tetraVerts)  4
testVertCountCube   = TestCase $ assertEqual "testVertCountCube"   (length cubeVerts)   8
testVertCountOcta   = TestCase $ assertEqual "testVertCountOcta"   (length octaVerts)   6
testVertCountDodeca = TestCase $ assertEqual "testVertCountDodeca" (length dodecaVerts) 20
testVertCountIcosa  = TestCase $ assertEqual "testVertCountIcosa"  (length icosaVerts)  12

testEdgeCountTetra  = TestCase $ assertEqual "testEdgeCountTetra"  (length tetraEdges)  (2 *  6)
testEdgeCountCube   = TestCase $ assertEqual "testEdgeCountCube"   (length cubeEdges)   (2 * 12)
testEdgeCountOcta   = TestCase $ assertEqual "testEdgeCountOcta"   (length octaEdges)   (2 * 12)
testEdgeCountDodeca = TestCase $ assertEqual "testEdgeCountDodeca" (length dodecaEdges) (2 * 30)
testEdgeCountIcosa  = TestCase $ assertEqual "testEdgeCountIcosa"  (length icosaEdges)  (2 * 30)

testFaceCountTetra  = TestCase $ assertEqual "testFaceCountTetra"  (length tetraFaces)  4
testFaceCountCube   = TestCase $ assertEqual "testFaceCountCube"   (length cubeFaces)   6
testFaceCountOcta   = TestCase $ assertEqual "testFaceCountOcta"   (length octaFaces)   8
testFaceCountDodeca = TestCase $ assertEqual "testFaceCountDodeca" (length dodecaFaces) 12
testFaceCountIcosa  = TestCase $ assertEqual "testFaceCountIcosa"  (length icosaFaces)  20

testPlatonicList    = TestList
                        [ TestLabel "testVertCountTetra"  testVertCountTetra
                        , TestLabel "testVertCountCube"   testVertCountCube
                        , TestLabel "testVertCountOcta"   testVertCountOcta
                        , TestLabel "testVertCountDodeca" testVertCountDodeca
                        , TestLabel "testVertCountIcosa"  testVertCountIcosa
                        -- ===== Edge Count Tests =====
                        , TestLabel "testEdgeCountTetra"  testEdgeCountTetra
                        , TestLabel "testEdgeCountCube"   testEdgeCountCube
                        , TestLabel "testEdgeCountOcta"   testEdgeCountOcta
                        , TestLabel "testEdgeCountDodeca" testEdgeCountDodeca
                        , TestLabel "testEdgeCountIcosa"  testEdgeCountIcosa
                        -- ===== Face Count Tests =====
                        , TestLabel "testFaceCountTetra"  testFaceCountTetra
                        , TestLabel "testFaceCountCube"   testFaceCountCube
                        , TestLabel "testFaceCountOcta"   testFaceCountOcta
                        , TestLabel "testFaceCountDodeca" testFaceCountDodeca
                        , TestLabel "testFaceCountIcosa"  testFaceCountIcosa
                        ]