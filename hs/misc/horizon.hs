module Horizon where


-- | radius of the earth in km
radius = 6371.0

-- | convert miles to km
milesToKm x = x * 1.609344

-- | convert km to miles
kilometersToMiles x = x * 0.621371192

-- | convert meters to inches
metersToInches x = x * 39.3700787

-- | convert meters to cm
metersToCm x = x * 100.0

-- | convert kilometers to inches
kilometersToInches x = metersToInches (x * 1000) 

-- | convert kilometers to cm
kilometersToCm x = metersToCm (x * 1000)

-- | distance from horizon in km given height in km
distanceKm :: Double -> Double
distanceKm heightKm = sqrt (2 * radius * heightKm)

-- | distance from horizon in km given height in meters
distance :: Double -> Double
distance h = 3.57 * sqrt h

-- | distance from true horizon in km given h in meters
distanceTH :: Double -> Double
distanceTH h = sqrt (13 * h)

-- | distance along curved surface in km given h in meters
curvedLengthH :: Double -> Double
curvedLengthH h = radius * acos (radius / (radius + h))

-- | distance along curved surface in km given d to horizon in meters
curvedLengthD :: Double -> Double
curvedLengthD d = radius * atan (d / radius)

-- | curvature per d kilometers from earth's surface (returns height in km)
curvature :: Double -> Double
curvature d = sqrt(radius**2 + d**2) - radius

-- | curvatures in centimeters for xs kilometers from earth's surface
curvaturesInCm :: [Double] -> [Double]
curvaturesInCm xs = map (kilometersToCm . curvature) xs

-- | curvatures in inches for xs miles from earth's surface
curvaturesInInches :: [Double] -> [Double]
curvaturesInInches xs = map (kilometersToInches . curvature . milesToKm ) xs

-- | curvatures in inches for xs miles from earth's surface
curvaturesInInchesRounded :: Integral b => [Double] -> [b]
curvaturesInInchesRounded xs = map round (curvaturesInInches xs)

-- | distance along curved surface
curvedLengthsDRounded :: Integral b => [Double] -> [b]
curvedLengthsDRounded xs = map (round . kilometersToMiles . curvedLengthD) xs

