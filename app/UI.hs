{-# LANGUAGE OverloadedStrings #-}

module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , defaultMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg, bg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Core as BC
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

import qualified Graphics.Vty as Co (blue, red, green, magenta, yellow, white)
import qualified Graphics.Vty as V

import Data.Monoid ((<>))

--  import Data.Sequence (Seq)
--  import qualified Data.Sequence as S

-- Types

-- This is our custom event that will be constantly fed into the app.
type Events = ()

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Direction = L | R
data Cube = Cube Direction

-- App definition

app :: App Cube Events Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  _ <- defaultMain app (Cube L)
  return ()

-- Handling events

handleEvent :: Cube -> BrickEvent Name Events -> EventM Name (Next Cube)
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ Cube L
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ Cube R

-- Drawing

fullBlock = '█'



square color = BC.withAttr color $ vLimit 3 $ hLimit 6 $ BC.fill fullBlock

drawCube = BC.vBox $ [
    BC.hBox $ [square blue, square yellow, square red],
    BC.hBox $ [square yellow, square yellow, square blue],
    BC.hBox $ [square red, square yellow, square red]
  ]

drawUI :: Cube -> [Widget Name]
drawUI (Cube L) = [C.center $ drawCube ]
drawUI (Cube R) = [C.center $ BC.withAttr red $ BC.txt "R"]

blue = "blue"
red = "red"
yellow = "yellow"

theMap :: AttrMap
theMap = attrMap V.defAttr [
          (blue, V.blue `on` V.blue),
          (red, V.red `on` V.red),
          (yellow, V.yellow `on` V.yellow)
          ]