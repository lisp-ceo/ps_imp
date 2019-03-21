module RayTracer.Tuples where

import Prelude

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

-- TODO implement addition for tuple types
--      this was a bad idea as the record pattern reflects user expectations around field
--      access moreso than destructuring a tagged union
-- https://github.com/purescript/documentation/blob/master/language/Type-Classes.md
-- XXX show
-- XXX semiring
-- XXX eq
--      this was *already* implemented thanks to the use of record syntax
