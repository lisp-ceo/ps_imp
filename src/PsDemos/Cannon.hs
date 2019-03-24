module PsDemos.Canon where

import Prelude
import RayTracer.Tuple (Tuple, point, vector)
import Effect (Effect, untilE)
import Effect.Console(log)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef

type Projectile = { position :: Tuple, velocity :: Tuple }
type Environment = { gravity :: Tuple, wind :: Tuple }

main :: Effect Unit
main = do
  let p = { position: point 0.0 0.0 0.0
          , velocity: vector 0.0 1000.0 0.0 }
  let e = { gravity: vector 0.0 (-9.7) 0.0
          , wind: vector 1.0 0.0 0.2 }
  log("initial p: " <> show p.position)
  let p' = simulate p e
  log("final p: " <> show p'.position)

simulate :: Projectile -> Environment -> Projectile
simulate p e = if shouldHalt p
               then p
               else simulate (tick p e) e

tick :: Projectile -> Environment -> Projectile
tick ({ position: p, velocity: v}) ({ gravity: g, wind: w}) = {position: p', velocity: v'}
  where
  p' = p + v
  v' = v + g + w

shouldHalt :: Projectile -> Boolean
shouldHalt ({ position: ({x: x, y: y, z: z, w: w}), velocity: v}) | y < 0.0 = true
                                                                  | otherwise = false
