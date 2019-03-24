module Main where

import Prelude
import Effect.Console(log)

main :: Effect Unit
main = do
  log("ps doesn't allow multiple executables :(")
