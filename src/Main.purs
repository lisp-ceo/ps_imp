module Main where

import Prelude
import Math(pi)

import RayTracer.Canvas(WithCanvas
                        ,reportResult)

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

main :: Effect Unit
main = do
     browserWindow <- window
     runExceptT writeToCanvas >>=
     reportResult browserWindow


-- TODO How to get rid of all these lifts?


