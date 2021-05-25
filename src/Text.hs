module Text where

import Core.Text.Rope
import Data.Map (Map)
import qualified Data.Map as Map

type Lines = Map Int Rope
type Pos = (Int, Int)
data Buffer = Buffer String Pos Int
  deriving (Eq, Show)
data Document = Document Pos Lines Buffer
  deriving (Eq, Show)

bufSize :: Int
bufSize = 10

newBuf :: Pos -> Buffer
newBuf p = Buffer "" p 0

newLines :: Lines
newLines = Map.fromList [(0, emptyRope)]

newDoc :: Document
newDoc = Document (0,0) newLines (newBuf (0,0))

insertAt :: Char -> String -> Int -> String
insertAt c s i = go s 0
  where go "" p = [c]
        go r@(x:xs) p = if i == p then c : r else x : go xs (p+1)

updMapWith :: (Rope -> Rope) -> Int -> Document -> Document
updMapWith f r d@(Document p m b) = Document p (Map.adjust f r m) b

insertBuffer :: Char -> Buffer -> Pos -> Buffer
-- Assumes the pos is within the buffer or right after
insertBuffer ch (Buffer s p@(br, bc) l) (r, c) = Buffer (insertAt ch s ip) p (l+1)
  where ip = c - bc

insBufAndNew :: Document -> Document
insBufAndNew d@(Document p@(r,c) m b@(Buffer s (br, bc) l))
  | l == 0    = d
  | otherwise = Document p 
                         (Map.adjust (\x -> insertRope bc (intoRope s) x) r m)
                         (newBuf p)

updPosAndBufPos :: (Int, Int) -> Document -> Document
updPosAndBufPos n@(nr,nc) (Document (r,c) m (Buffer s (br, bc) l)) =
  Document n m (Buffer s n l)

delLineRange :: (Int, Int) -> Int -> Document -> Document
-- Endpoints are inclusive, requires a valid, non trivial interval
delLineRange (ic, ec) r d@(Document p@(dr,dc) m b) = 
  if dr == r && dc >= ic
    then updPosAndBufPos (dr,dc-(ec-ic)) res
    else res
  where f :: Rope -> Rope
        f r = (\(x,y) -> x <> snd (splitRope (ec) y)) $ splitRope ic r
        res = updMapWith f r (insBufAndNew d)

insertDoc :: Char -> Document -> Document
insertDoc ch d@(Document p@(r,c) m b@(Buffer s (br, bc) l))
  | l < bufSize = (Document (r,c+1) m (insertBuffer ch b p))
  | otherwise   = insertDoc ch (insBufAndNew d)
