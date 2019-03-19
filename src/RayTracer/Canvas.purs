module RayTracer.Canvas where

import Prelude
import Math(pi)
import Control.Monad.Except (ExceptT, lift, throwError, runExceptT)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Graphics.Canvas (getCanvasElementById
                       ,getContext2D
                       ,rect
                       ,arc
                       ,setFillStyle
                       ,fillPath
                       )
import Web.HTML(window
               ,Window)
import Web.HTML.Window(alert)

type WithCanvas = ExceptT String Effect String

reportResult :: Window -> Either String String -> Effect Unit
reportResult window (Right msg) = alert msg window
reportResult window (Left err) = alert err window
