module RayTracer.Canvas where

import Control.Monad.Except (ExceptT)

type WithCanvas = ExceptT String Effect String

reportResult :: Window -> Either String String -> Effect Unit
reportResult window (Right msg) = alert msg window
reportResult window (Left err) = alert err window
