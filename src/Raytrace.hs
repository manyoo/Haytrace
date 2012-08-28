module Raytrace (raytrace)
    where

import Vector
import Data
import Data.Maybe (fromJust)

getDirection s e = normalize $ e <-> s

-- now the camera is not fixed on a point. the view is not natural if fixed on one point.
raytrace :: Int -> Scene -> [Color]
raytrace dist scene =
    [render (camera x y) (coord x y) scene| y <- [yHigh,yHigh - 1 .. yLow], x <- [xLow .. xHigh]]
    where coord x y = (fromIntegral x, fromIntegral y, 0.0)
          camera x y = (fromIntegral x, fromIntegral y, negate $ fromIntegral dist)
          xLow = negate (width scene `div` 2)
          xHigh = xLow + width scene - 1
          yLow = negate (height scene `div` 2)
          yHigh = yLow + height scene - 1

render :: Coord -> Coord -> Scene -> Color
render camera point scene = toColor $ rayTraceHere ray scene 1.0
    where ray = Ray { origin = camera, direction = getDirection camera point }

-- not sure about this implementation, but it works well.
toColor :: (Intensity, Color) -> Color
toColor (intens, color) = scale color intens

-- the core function. recursive with onPoint.
rayTraceHere :: Ray -> Scene -> Double -> (Intensity, Color)
rayTraceHere ray scene contribution 
    | contribution < 0.1 = (defaultAmbient, backgroundColor scene)
    | otherwise = let objs = objects scene
                      ls = lights scene
                      fstObj = firstObject scene ray
                  in case fstObj of
                       Nothing -> (defaultAmbient,backgroundColor scene)
                       Just (t, intersectPoint, obj) -> 
                           onPoint intersectPoint obj ray scene contribution

onPoint :: Coord -> Object -> Ray -> Scene -> Double -> (Intensity,Color)
onPoint p obj ray scene contribution = 
    let localIntensity = localIntensityOfPoint p obj ray scene
        (refIntensity,refColor) = rayTraceHere reflectRay scene (contribution * refCoef)
        (transIntensity,transColor) = rayTraceHere trasmitRay scene (contribution * transCoef)
        refCoef = sReflection surf
        transCoef = sTransmit surf
        surf = surface obj
        localColor = sColor surf
        reflectRay = Ray {origin = p, direction = reflectDirection}
        trasmitRay = Ray {origin = p, direction = transmitDirection}
        v = neg $ direction ray
        n0 = getNormal p obj
        v' = scale v $ 1.0 / abs (n0 <.> v)
        reflectDirection = normalize $ scale n0 2 <-> v'
        transmitDirection = normalize $ scale (n0 <-> v') transCoef <-> n0 -- problem here!!!
        sumColor c1 c2 c3 = clips $ scale c1 localIntensity <+> scale c2 refIntensity
                            <+> scale c3 transIntensity
    in (clip $ localIntensity + refIntensity * refCoef + transIntensity * transCoef,
        sumColor localColor refColor transColor)

clip x | x > 1 = 1
       | x < 0 = 0
       | otherwise = x

-- 'phoone' local light model
localIntensityOfPoint :: Coord -> Object -> Ray -> Scene -> Intensity
localIntensityOfPoint p obj ray scene = 
    let ls = filter (noShadow p scene obj) $ lights scene
        s = sum $ map (calcIntensity p obj ray) ls
    in defaultAmbient * sAmbient (surface obj) + s

calcIntensity :: Coord -> Object -> Ray -> Light -> Intensity
calcIntensity p obj ray light = 
    let n0 = getNormal p obj
        l0 = getDirection p (position light)
        cos_i = l0 <.> n0
        r0 = scale n0 (2 * cos_i) <-> l0
        v0 = neg (direction ray)
        cos_theta = r0 <.> v0
        surf = surface obj
        intens = intensity light
    in sDiffuse surf * intens * cos_i + sSpecular surf * intens * (cos_theta ^ sShininess surf)

-- check whether point p on obj is shadowed or not, if seen from light.
noShadow :: Coord -> Scene -> Object -> Light -> Bool
noShadow p scene obj light = 
    let ray = Ray {origin = p, direction = getDirection p lp}
        lp = position light
        intersectedObj =map fromJust $ filter notNothing $ map (intersectPoint ray) $ objects scene
        notNothing Nothing = False
        notNothing (Just _) = True
        (t,ip,o) = head intersectedObj
        n = getNormal p obj
        dotp = n <.> getDirection p lp  -- this is important. then we don't have to care about
    in if length intersectedObj > 1     -- the f**king errors in floating computation.
       then False
       else if length intersectedObj == 1
            then o == obj && dotp > 0
            else True
