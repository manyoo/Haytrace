module Data
    where

import Vector
import Data.Maybe (fromJust)

type Coord = Vector  -- Coordination of a point in the space
type Color = Vector  -- Colors are of (R,G,B) values
type Intensity = Double -- the Intensity of a light

data Surface = Surface { sAmbient :: Double, -- ambient coefficient of the surface
                         sDiffuse :: Double, -- diffuse 
                         sSpecular :: Double, -- specular 
                         sShininess :: Int,  -- used for specular
                         sReflection :: Double, 
                         sTransmit :: Double,
                         sColor :: Color
                       }
               deriving (Show, Eq, Ord)

data Object = Sphere { radius :: Double,
                       center :: Coord,
                       surface :: Surface
                     }
              deriving (Show, Eq, Ord)

-- now we assume all lights are white.
data Light = Light { position :: Coord,
                     intensity :: Intensity
                   }
             deriving Show
data Ray = Ray { origin :: Coord,
                 direction :: Vector -- make sure this is normalized
               }
           deriving Show
data Scene = Scene { objects :: [Object],
                     lights :: [Light],
                     backgroundColor :: Color,
                     width :: Int,
                     height :: Int
                   }
             deriving Show

defaultSurface = Surface { sAmbient = 0.3,
                           sDiffuse = 0.4,
                           sSpecular = 0.6,
                           sShininess = 10,
                           sReflection = 0.2,
                           sTransmit = 0,
                           sColor = (0.3,0.8,0.9)
                         }

defaultBackgroundColor = (0.0, 0.0, 0.0)  -- default background color
defaultAmbient = 1.0
zeroVector = (0.0, 0.0, 0.0)
imageWidth,imageHeight :: Int
imageWidth = 1024
imageHeight = 768  -- the default size of end image

getNormal :: Coord -> Object -> Coord
getNormal point Sphere{radius = r, center = c} =
    normalize (point <-> c)

-- don't change if not sure. really fussy here due to the floating computation errors.
intersectPoint :: Ray -> Object -> Maybe (Double, Coord, Object)
intersectPoint ray obj@Sphere{radius = r, center = (xc,yc,zc)} = 
    let r0 = origin ray
        (x0,y0,z0) = r0
        rd = direction ray
        (xd,yd,zd) = rd
        b = 2 * ((x0-xc)*xd + (y0-yc)*yd + (z0-zc)*zd)
        c = (x0-xc)**2 + (y0-yc)**2 + (z0-zc)**2 - r**2
        b2_4ac = b**2 - 4*c
        t0 = ((-b) - sqrt b2_4ac) * 0.5
        t1 = ((-b) + sqrt b2_4ac) * 0.5
        t = min t0 t1
        point t = r0 <+> scale rd t
    in if b2_4ac < 0 || (t0 < 0 && t1 < 0)
       then Nothing
       else if t0 > 0 && t1 > 0 && not (nearZero t0) && not (nearZero t1)
            then Just (t, point t, obj)
            else if t0 > 0 && not (nearZero t0)
                 then Just (t0, point t0, obj)
                 else if t1 > 0 && not (nearZero t1)
                      then Just (t1, point t1, obj)
                      else Nothing

nearZero x | abs x < 0.0001 = True
           | otherwise = False

-- find the nearest intersected object of a ray
firstObject :: Scene -> Ray -> Maybe (Double, Coord, Object)
firstObject scene ray = 
    let intersectedObj = map fromJust $ filter notNothing $ map (intersectPoint ray) $ objects scene
        notNothing Nothing = False
        notNothing (Just _) = True
    in case intersectedObj of
         [] -> Nothing
         otherwise -> Just $ minimum intersectedObj

clips :: Color -> Color
clips (r,g,b) = (clip r, clip g, clip b)
    where clip n | n < 0 = 0
                 | n > 1 = 1
                 | otherwise = n
