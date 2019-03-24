module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)


main :: Effect Unit
main = do
  specs <- discover "Test\\..*Spec" -- TODO move to raytracer namespace to stop running framework specs
  run [consoleReporter] specs
