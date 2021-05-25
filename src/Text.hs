module Text where

-- TODO save the cursor pos before it is forced to the end of a line,
-- undo functionality.

import Core.Text.Rope
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type Lines = Map Int Rope
type Pos = (Int, Int)
data Buffer = Buffer String Pos Int
  deriving (Eq, Show)
data Document = Document Pos Lines Buffer
  deriving (Eq, Show)
data Direction = L | R | U | D deriving (Eq, Show)

getDoc :: [String] -> IO Document
getDoc args = 
  if length args == 0
     then pure newDoc
     else fileToDoc (head args)

strToDoc :: String -> Document
strToDoc s =
  Document
    (0,0)
    (snd (foldl (\(i, m) x -> (i+1, Map.insert i (intoRope x) m))
                (0 :: Int, Map.empty) (splitStr '\n' s)))
    (newBuf (0,0))

fileToDoc :: String -> IO Document
fileToDoc s = do
  str <- readFile s
  print $ splitStr '\n' str
  let res = strToDoc str
  return res

splitStr :: Char -> String -> [String]
splitStr c s = splitStr' s c ""
  where splitStr' "" c acc = if acc == "" then [] else [reverse acc]
        splitStr' (x:xs) c acc = if x == c
                                   then (reverse acc) : splitStr' xs c ""
                                   else splitStr' xs c (x:acc)

fromDoc :: Document -> String
fromDoc = fromRope . (foldMap (\x -> x <> intoRope "\n")) . getLines . insBufAndNew

bufSize :: Int
bufSize = 10

newBuf :: Pos -> Buffer
newBuf p = Buffer "" p 0

getLines :: Document -> Lines
getLines (Document p l b) = l

getCurrRope :: Document -> Rope
getCurrRope (Document (cr, cc) l b) = fromJust $ Map.lookup cr l

newLines :: Lines
newLines = Map.fromList [(0, emptyRope)]

newDoc :: Document
newDoc = Document (0,0) newLines (newBuf (0,0))

testDoc2 :: Document
testDoc2 = Document 
                 (2,0)
                 (Map.fromList [ (0, intoRope "hi my name is")
                               , (1, intoRope "Evan!!")
                               , (2, emptyRope) ])
                 (newBuf (2,0))

testDoc :: Document
testDoc = Document (0,7) (Map.fromList [(0,intoRope "hi my ")]) (Buffer "n" (0,6) 1)

insertAt :: Char -> String -> Int -> String
insertAt c s i = go s 0
  where go "" p = [c]
        go r@(x:xs) p = if i == p then c : r else x : go xs (p+1)

updMapWith :: (Rope -> Rope) -> Int -> Document -> Document
updMapWith f r d@(Document p m b) = Document p (Map.adjust f r m) b

delLine :: Int -> Document -> Document
delLine r d@(Document p m b) = Document p 
                                        (Map.mapKeys
                                          (\k -> if k > r then k - 1 else k)
                                          (Map.deleteAt r m))
                                        b

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
-- Endpoints are inclusive
delLineRange (ic, ec) r d@(Document p@(dr,dc) m b) = 
  if dr == r && dc >= ic
    then updPosAndBufPos (dr,dc-(ec+1-ic)) res
    else res
  where f :: Rope -> Rope
        f r = (\(x,y) -> x <> snd (splitRope (ec+1-ic) y)) $ splitRope ic r
        res = updMapWith f r (insBufAndNew d)

insertDoc :: Char -> Document -> Document
insertDoc ch d@(Document p@(r,c) m b@(Buffer s (br, bc) l))
  | l < bufSize = (Document (r,c+1) m (insertBuffer ch b p))
  | otherwise   = insertDoc ch (insBufAndNew d)

appendLines :: Int -> Int -> Document -> Document
appendLines r1 r2 d@(Document p@(r,c) m b@(Buffer s (br, bc) l)) =
  updMapWith (\x -> x <> fromJust (Map.lookup r2 m)) r1 d 

delOne :: Document -> Document
delOne d@(Document p@(r,c) m b@(Buffer s (br, bc) l))
  | c == 0 && r /= 0 = delLine r (appendLines (r-1) r (moveDir d U))
  | c == 0 && r == 0 = d
  | otherwise        = delLineRange (c-1,c-1) r d

endOfLine :: Document -> Int -> Int
endOfLine d@(Document p@(r,c) m b@(Buffer s (br, bc) l)) i =
  widthRope $ fromJust (Map.lookup i m)

moveDir :: Document -> Direction -> Document
moveDir d@(Document p@(r,c) m b@(Buffer s (br, bc) l)) dir =
  updPosAndBufPos (nr,nc) nd
  where nd = insBufAndNew d
        (nr,nc) = case dir of
                    L -> if c == 0 
                           then p
                           else (r,c-1)
                    R -> if c == endOfLine nd r
                           then p
                           else (r,c+1)
                    U -> if r == 0
                           then p
                           else (r-1, let nc = endOfLine nd (r-1) in
                                        if c <= nc
                                          then c
                                          else nc)
                    D -> if r == (fst (Map.findMax m))
                           then p
                           else (r+1, let nc = endOfLine nd (r+1) in
                                        if c <= nc
                                          then c
                                          else nc)

docMapKeys :: (Int -> Int) -> Document -> Document
docMapKeys f (Document p m b) = Document p (Map.mapKeys f m) b

docInsert :: Int -> Rope -> Document -> Document
docInsert k v (Document p m b) = Document p (Map.insert k v m) b

docInsertWith :: (Rope -> Rope -> Rope) -> Int -> Rope -> Document -> Document
docInsertWith f k v (Document p m b) = Document p (Map.insertWith f k v m) b

insLine :: Document -> Document
insLine d@(Document p@(r,c) m b@(Buffer s (br, bc) l)) =
  docInsertWith const r ol $ docInsert (r+1) nl $
    docMapKeys 
      (\x -> if x > r then x + 1 else x) 
      $ updPosAndBufPos (r+1,0) nd
  where nd = insBufAndNew d 
        (ol, nl) = splitRope c $ getCurrRope nd
