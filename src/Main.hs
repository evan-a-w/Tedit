module Main where

import Text
import System.Environment
import Brick
import qualified Graphics.Vty as V

type Name = ()

main :: IO ()
main = do
  args <- getArgs
  doc <- getDoc args
  finalDoc <- defaultMain app doc
  saveNoRes finalDoc

app :: App Document e Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor --chooseCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

handleEvent :: Document -> BrickEvent Name e -> EventM Name (Next Document)
handleEvent d (VtyEvent (V.EvKey V.KUp []))        = continue $ moveDir d U
handleEvent d (VtyEvent (V.EvKey V.KDown []))      = continue $ moveDir d D
handleEvent d (VtyEvent (V.EvKey V.KRight []))     = continue $ moveDir d R
handleEvent d (VtyEvent (V.EvKey V.KLeft []))      = continue $ moveDir d L
handleEvent d (VtyEvent (V.EvKey (V.KChar c) []))  = continue $ insertDoc c d
handleEvent d (VtyEvent (V.EvKey (V.KEnter) []))   = continue $ insLine d
handleEvent d (VtyEvent (V.EvKey (V.KBS) []))      = continue $ delOne d
handleEvent d (VtyEvent (V.EvKey (V.KDel) []))     = continue $ delOne d
handleEvent d (VtyEvent (V.EvKey (V.KBackTab) [])) = continue $ insTab d
handleEvent d (VtyEvent (V.EvKey (V.KEsc) []))     = halt d
--handleEvent d (VtyEvent (V.EvKey (V.KIns) []))     = liftIO (saveDoc d) >>= continue
-- this function cannot be found despite there being an instance for it
handleEvent d _                                    = continue d

chooseCursor :: Document -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor = undefined

drawUI :: Document -> [Widget Name]
drawUI d = [strWrap $ fromDoc d]

textBox :: AttrName
textBox = attrName "textBox"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (textBox, V.defAttr) ]
