module PsDemos.CanvasDraw where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas (getCanvasElementById
                       ,getContext2D
                       ,rect
                       ,setFillStyle
                       ,fillPath)
import Web.HTML(window)
import Web.HTML.Window(alert)

main :: Effect Unit
main = do
     gWindow <- window
     maybeCanvas <- getCanvasElementById "canvas"
     case maybeCanvas of
       Nothing -> alert "Cannot find canvas" gWindow
       Just canvas -> do
         ctx <- getContext2D canvas
         _ <- setFillStyle ctx "#0000FF"
         fillPath ctx $ rect ctx
           { x: 250.0
           , y: 250.0
           , width: 100.0
           , height: 100.0
           }





-- module Main where

-- import Prelude

-- import Effect (Effect)
-- import Data.Maybe
-- import Graphics.Canvas (CanvasElement
-- , rect
-- , fillPath
-- , setFillStyle
-- , getContext2D
-- , getCanvasElementById
-- )

-- import Partial.Unsafe (unsafePartial)

-- main :: Effect (canvas :: CanvasElement) Unit
-- main = void $ unsafePartial do
-- Just canvas <- getCanvasElementById "canvas"
-- ctx <- getContext2D canvas
-- _ <- setFillStyle "#0000FF" ctx
-- fillPath ctx $ rect ctx
-- { x: 250.0
-- , y: 250.0
-- , w: 100.0
-- , h: 100.0
-- }
