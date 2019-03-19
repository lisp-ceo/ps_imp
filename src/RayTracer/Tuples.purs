module RayTracer.Tuples where

import Prelude

-- Book represents tuples using w, does not have ADTs
-- TODO update examples to use ADTs
type Tuple = { x :: Number, y :: Number, z :: Number, w :: Number }

tuple :: Number -> Number -> Number -> Number -> Tuple
tuple x y z w = { x: x, y: y, z: z, w: w }

point :: Tuple -> Boolean
point ({x: _, y: _, z: _, w: 1.0}) = true
point _ = false
