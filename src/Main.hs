module Main where

import Text
import System.Environment
import Brick
import qualified Graphics.Vty as V
import Control.Monad.IO.Class

data Name = Leno deriving (Ord, Eq)

main :: IO ()
main = do
  args <- getArgs
  doc <- getDoc args
  finalDoc <- defaultMain app doc
  saveNoRes finalDoc
  putStrLn $ show $ getSPos finalDoc
  putStrLn $ show $ getWidth finalDoc
  putStrLn $ show $ getPos finalDoc

app :: App Document e Name
app = App { appDraw         = drawUI
          , appChooseCursor = chooseCursor
          , appHandleEvent  = hEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

hEvent :: Document -> BrickEvent Name e -> EventM Name (Next Document)
hEvent d ev = do
  mExtent <- lookupExtent Leno
  case mExtent of
    Nothing -> halt d
    Just (Extent _ _ (width, height)) -> handleEvent (setHW (height, width) d) ev

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
handleEvent d (VtyEvent (V.EvKey (V.KIns) []))     = liftIO (saveDoc d) >>= continue
handleEvent d (VtyEvent (V.EvKey (V.KHome) []))    = liftIO (saveDoc d) >>= continue
handleEvent d _                                    = continue d

chooseCursor :: Document -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor d cs = Just (head cs)

drawUI :: Document -> [Widget Name]
drawUI d = [ showCursor Leno (getCursor d) $ reportExtent Leno $ str $ fromDoc d ]

textBox :: AttrName
textBox = attrName "textBox"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (textBox, V.defAttr) ]

getCursor :: Document -> Location
getCursor d = Location $ let (c, r) = invert $ getPos d 
                             width = getWidth d in
                             if width == 0 then (0, r) else
                             if c >= width-1
                                then (width-1, r)
                                else (c, r)
  where invert (a,b) = (b,1+a-getSPos d) 
