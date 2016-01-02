module Math (minQuadraticRoot) where

minQuadraticRoot :: Double -> Double -> Double -> Maybe Double
minQuadraticRoot a b c
    | (isZero a) && (isZero b) = Nothing
    | (isZero a) && (isPos  b) = Just (-c / b)
    | isPos disc               = Just (min ((-b+sqrtDisc)/(2*a))
                                           ((-b-sqrtDisc)/(2*a))
                                      )
    | otherwise                = Nothing
    where disc     = b**2 - 4*a*c
          sqrtDisc = sqrt disc
          eps      = 0.0001  -- TODO: Revisit
          isPos x  = x > eps
          isZero x = abs x < eps

