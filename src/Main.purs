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

writeToCanvas :: WithCanvas
writeToCanvas = do
  maybeCanvas <- lift $ getCanvasElementById "canvas"
  case maybeCanvas of
    Nothing -> throwError "Unable to find canvas"
    Just canvas -> do
      ctx <- lift $ getContext2D canvas
      _ <- lift $ setFillStyle ctx "#0000FF"
      _ <- lift $ fillPath ctx $ rect ctx { x: 250.0, y: 250.0, width: 100.0, height: 100.0}
      _ <- lift $ setFillStyle ctx "#00FF00"
      _ <- lift $ fillPath ctx $ arc ctx { x: 225.0, y: 225.0, radius: 50.0, start: pi * 5.0 / 8.0, end: pi * 2.0}
      pure "That worked!"
