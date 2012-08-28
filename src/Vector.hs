module Vector
    where

-- Vector mathematics / Vector Calculus Code

type Vector = (Double, Double, Double)

originPoint = (0.0, 0.0, 0.0)

-- invert an vector
invert :: Vector -> Vector
invert (x,y,z) = (negate x, negate y, negate z)

-- maginitude of a vector
magnitude :: Vector -> Double
magnitude (x,y,z) = sqrt (x*x + y*y + z*z)
-- sometimes we don't need the square-rooted magnitude, so this one is faster
squareMagnitude (x,y,z) = x*x + y*y + z*z

-- get the unit vector by normalization
normalize v = scale v $ 1.0 / magnitude v

scale (x,y,z) s = (x*s, y*s, z*s)

add (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
(<+>) :: Vector -> Vector -> Vector
(<+>) = add

substract (x1,y1,z1) (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)
(<->) :: Vector -> Vector -> Vector
(<->) = substract

neg (x, y, z) = (negate x, negate y, negate z)

-- this function is simple enough to write it explicitly so it maybe more efficient
addScaledVector v1 v2 = add v1 . scale v2

innerProduct (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2
(<.>) :: Vector -> Vector -> Double
(<.>) = innerProduct

crossProduct (x1,y1,z1) (x2,y2,z2) = (x1*x2, y1*y2, z1*z2)
(<*>) :: Vector -> Vector -> Vector
(<*>) = crossProduct
