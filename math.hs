module Math (quadraticRoots) where

quadraticRoots :: Double -> Double -> Double -> [Double]
quadraticRoots a b c
    | (isZero a) && (isZero b) = []
    | (isZero a) && (isPos  b) = [-c / b]
    | isPos disc               = [ (-b+sqrtDisc) / (2*a)
                                 , (-b-sqrtDisc) / (2*a)
                                 ]
    | otherwise                = []
    where disc     = b**2 - 4*a*c
          sqrtDisc = sqrt disc
          eps      = 0.0001  -- TODO: Revisit
          isPos x  = x > eps
          isZero x = abs x < eps