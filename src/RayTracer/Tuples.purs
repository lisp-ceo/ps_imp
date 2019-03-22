module RayTracer.Tuples where

import Prelude
import Math (sqrt,abs)
import Data.Maybe (Maybe(..))
import Data.Array ((!!))
import Data.Foldable (all, foldl)

-- Book represents tuples using w, does not have ADTs
-- XXX update examples to use ADTs
--     this was a bad idea as the record pattern reflects user expectations around field
--     access moreso than destructuring a tagged union
-- data Tuple = Point Number Number Number
--            | Vector Number Number Number
type Tuple = { x :: Number, y :: Number, z :: Number, w :: Number }

tuple :: Number -> Number -> Number -> Number -> Tuple
tuple x y z w = { x: x, y: y, z: z, w: w }

isPoint :: Tuple -> Boolean
isPoint ({x: _, y: _, z: _, w: 1.0}) = true
isPoint _ = false

isVector :: Tuple -> Boolean
isVector ({x: _, y: _, z: _, w: 0.0}) = true
isVector _ = false

point :: Number -> Number -> Number -> Tuple
point x y z = { x: x, y: y, z: z, w: 1.0 }

vector :: Number -> Number -> Number -> Tuple
vector x y z = { x: x, y: y, z: z, w: 0.0 }

-- scalar multiplication
smult :: Tuple -> Number -> Tuple
smult ({x: x, y: y, z: z, w: w}) n = {x: x*n, y: y*n, z: z*n, w: w*n }

-- scalar division
sdiv :: Tuple -> Number -> Tuple
sdiv ({x: x, y: y, z: z, w: w}) n = {x: x/n, y: y/n, z: z/n, w: w/n }

magnitude :: Tuple -> Number
magnitude ({x: x, y: y, z: z, w: w}) = sqrt(sum_of_squares)
  where
  sum_of_squares = x*x + y*y + z*z + w*w

normalize :: Tuple -> Tuple
normalize t = {x: x', y: y', z: z', w: w'}
  where
  t' = magnitude t
  x' = t.x / t'
  y' = t.y / t'
  z' = t.z / t'
  w' = t.w / t'

-- piecewise :: forall t. (Applicative t, Semigroup t) => Tuple -> Tuple -> t Number
-- piecewise t1 t2 = pure t1.x <> pure t2.x
piecewise :: Tuple -> Tuple -> Array (Array Number)
piecewise t1 t2 = [[t1.x, t2.x]
                  , [t1.y, t2.y]
                  , [t1.z, t2.z]
                  , [t1.w, t2.w]]

etaCompare :: Tuple -> Tuple -> Number -> Boolean
etaCompare t1 t2 eta = all (\a -> maybeEqRelative eta (a !! 0) (a !! 1)) (piecewise t1 t2)

-- XXX use Data.Numbers
-- eqRelative :: Number -> Number -> Fraction
-- do not wish to introduce new types just for this
-- requires use of constructor from 3rd party package
maybeEqRelative :: Number -> Maybe Number -> Maybe Number -> Boolean
maybeEqRelative eta (Just n1) (Just n2) = eqRelative eta n1 n2
maybeEqRelative eta _ _ = false

-- TODO fmap/apply instead of handling maybes
maybeMult :: Maybe Number -> Maybe Number -> Number
-- maybeMult :: Maybe Number -> Maybe Number -> Maybe Number
-- maybeMult a b = (*) <$> a <*> b
maybeMult (Just n1) (Just n2) = n1 * n2
maybeMult _ _ = 0.0

eqRelative :: Number -> Number -> Number -> Boolean
eqRelative eta a b = abs(a - b) <= eta

dot :: Tuple -> Tuple -> Number
dot t1 t2 = foldl (+) 0.0 (piecewiseMult t1 t2)

piecewiseMult :: Tuple -> Tuple -> Array Number
piecewiseMult t1 t2 = map (\a -> maybeMult (a !! 0) (a !! 1)) (piecewise t1 t2)
