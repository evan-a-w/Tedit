module Text where

-- TODO undo functionality, proper text wrapping without bugs

import Core.Text.Rope
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import System.Directory

type Lines = Map Int Rope
type Pos = (Int, Int)
data Buffer = Buffer String Pos Int
  deriving (Eq, Show)
data Document = Document Pos Lines Buffer
  deriving (Eq, Show)
data Direction = L | R | U | D deriving (Eq, Show)

saveDoc :: Document -> IO Document
saveDoc d = do
  writeFile (getDocName d) (fromDocStart d)
  return d

printSPos :: Document -> IO Document
printSPos d = do
  putStrLn $ show $ getSPos d
  return d

getDocName :: Document -> String
getDocName = fromRope . (fromMaybe (intoRope "default.txt")) . (Map.lookup (-1)) . getLines

saveNoRes :: Document -> IO ()
saveNoRes d = writeFile (getDocName d) (fromDocStart d)

posTuple :: Pos -> (Int, Int)
posTuple (a,b) = (a,b)

getPos :: Document -> (Int, Int)
getPos (Document p l b) = posTuple p

getDoc :: [String] -> IO Document
getDoc args = 
  if length args == 0
     then return newDoc
     else fileToDoc (head args)

strToDoc :: String -> Document
strToDoc s =
  Document
    (0,0)
    (snd (foldl (\(i, m) x -> (i+1, Map.insert i (intoRope x) m))
                (0 :: Int, Map.empty) (splitStr '\n' s)))
    (newBuf (0,0))

unionDoc :: Document -> Lines -> Document
unionDoc (Document p l b) l2 =
  Document p
           (Map.unionWith const l l2)
           b

fileToDoc :: String -> IO Document
fileToDoc s = (doesFileExist s) >>= (\x ->
  if x
     then do str <- readFile s
             let res = strToDoc str
             return $ unionDoc res $ defaultLines s
     else return (docNamed s))

splitStr :: Char -> String -> [String]
splitStr c s = splitStr' s c ""
  where splitStr' "" c acc = if acc == "" then [] else [reverse acc]
        splitStr' (x:xs) c acc = if x == c
                                   then (reverse acc) : splitStr' xs c ""
                                   else splitStr' xs c (x:acc)

fromDoc :: Document -> String
fromDoc d = fromRope $ intoRope "Document name: " <> long (lookupDoc (-1) nd) <> mText
  where sp = getSPos d
        nd = insBufAndNew d
        mText = Map.foldrWithKey (\k x y -> if k < sp
                                               then y
                                               else (x <> (intoRope "\n")) <> y)
               (intoRope "") $ getLines nd
        long rope = rope <> intoRope (take 1000 $ repeat ' ') <> intoRope "\n" 

fromDocStart :: Document -> String
fromDocStart d = (fromRope . (Map.foldrWithKey (\k x y ->
                        if k < 0
                          then y
                          else (x <> (intoRope "\n")) <> y) (intoRope ""))
          . getLines . insBufAndNew) d

bufSize :: Int
bufSize = 10

newBuf :: Pos -> Buffer
newBuf p = Buffer "" p 0

getLines :: Document -> Lines
getLines (Document p l b) = l

getCurrRope :: Document -> Rope
getCurrRope (Document (cr, cc) l b) = fromJust $ Map.lookup cr l

defaultLines :: String -> Lines
defaultLines s = Map.fromList [(-5, intoRope "0"), (-4, intoRope "0"), (-3, intoRope "0"), (-2, intoRope "0"), (-1, intoRope s)]

newLines :: String -> Lines
newLines s = Map.fromList [(-5, intoRope "0"), (-4, intoRope "0"), (-3, intoRope "0"), (-2, intoRope "0"), (-1, intoRope s), (0, emptyRope)]

newDoc :: Document
newDoc = Document (0,0) (newLines "default.txt") (newBuf (0,0))

docNamed :: String -> Document
docNamed s = Document (0,0) (newLines s) (newBuf (0,0))

testDoc2 :: Document
testDoc2 = Document 
                 (1,0)
                 (Map.fromList [ ((-1), intoRope "test.txt")
                               , (0, intoRope "hi my name is")
                               , (1, intoRope "Evan!!")
                               , (2, emptyRope) ])
                 (newBuf (1,0))
d2 = moveDir (setSPos 1 (setHeight 1 testDoc2)) D

testDoc :: Document
testDoc = Document (0,7) (Map.fromList [(0,intoRope "hi my ")]) (Buffer "n" (0,6) 1)

insertAt :: Char -> String -> Int -> String
insertAt c s i = go s 0
  where go "" p = [c]
        go r@(x:xs) p = if i == p then c : r else x : go xs (p+1)

updMapWith :: (Rope -> Rope) -> Int -> Document -> Document
updMapWith f r d@(Document p m b) = Document p (Map.adjust f r m) b

delLine :: Int -> Document -> Document
delLine r d@(Document p m b) = 
  if Map.lookup r dm == Nothing then
    Document p 
    (Map.mapKeys
    (\k -> if k > r then k - 1 else k)
    dm)
    b
  else undefined
  where dm = Map.delete r m

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

supUpd :: (Int, Int) -> Document -> Document
supUpd n@(nr,nc) (Document (r,c) m (Buffer s (br, bc) l)) =
  setHPos nc (Document n m (Buffer s n l))

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
appendLines r1 r2 d = updMapWith (\x -> x <> l2) r1 d
  where l2 = lookupDoc r2 d

appendLine :: Int -> Rope -> Document -> Document
appendLine r1 s d = updMapWith (\x -> x <> s) r1 d

delOne :: Document -> Document
delOne d@(Document p@(r,c) m b@(Buffer s (br, bc) l))
  | c == 0 && r /= 0 = let x = lookupDoc r d in
                           if x == emptyRope then moveEndLine (r-1) (delLine r d)
                           else appendLine (r-1) x (moveEndLine (r-1) (delLine r d))
  | c == 0 && r == 0 = d
  | otherwise        = delLineRange (c-1,c-1) r d

endOfLine :: Document -> Int -> Int
endOfLine d@(Document p@(r,c) m b@(Buffer s (br, bc) l)) i =
  widthRope $ fromJust (Map.lookup i m)

moveEndLine :: Int -> Document -> Document
moveEndLine r d = updPosAndBufPos (r, nc) nd
  where nd = insBufAndNew d
        nc = endOfLine nd r

setHPos :: Int -> Document -> Document
setHPos i = docSet (-2) (intoRope $ show i)

getHPos :: Document -> Int
getHPos (Document p l b) = case Map.lookup (-2) l of
                             Nothing -> 0
                             Just r  -> (read . fromRope) r

setSPos :: Int -> Document -> Document
setSPos i = docSet (-3) (intoRope $ show i)

setHeight :: Int -> Document -> Document
setHeight i = docSet (-4) (intoRope $ show i)

getHeight :: Document -> Int
getHeight (Document p l b) = case Map.lookup (-4) l of
                             Nothing -> 0
                             Just r  -> (read . fromRope) r

setWidth :: Int -> Document -> Document
setWidth i = docSet (-5) (intoRope $ show i)

getWidth :: Document -> Int
getWidth (Document p l b) = case Map.lookup (-5) l of
                             Nothing -> 0
                             Just r  -> (read . fromRope) r

setHW :: (Int, Int) -> Document -> Document
setHW (h, w) d = setWidth w $ setHeight h d

getSPos :: Document -> Int
getSPos (Document p l b) = case Map.lookup (-3) l of
                             Nothing -> 0
                             Just r  -> (read . fromRope) r

docSet :: Int -> Rope -> Document -> Document
docSet i r d@(Document p m l) =
  Document 
    p
    (Map.alter (const (Just r)) i m)
    l

moveDir :: Document -> Direction -> Document
moveDir d@(Document p@(r,c) m b@(Buffer s (br, bc) l)) dir =
  case dir of
    L -> if c == 0 
           then nd
           else supUpd (r,c-1) nd
    R -> if c == endOfLine nd r
           then nd
           else supUpd (r,c+1) nd
    U -> if r == 0
           then nd
           else let res = updPosAndBufPos 
                          (r-1, let nc = endOfLine nd (r-1) in
                                  if pc <= nc
                                    then pc
                                    else nc) nd in
                  let g = sp in if g == r
                                     then setSPos (g - 1) res
                                     else res
    D -> if r == (fst (Map.findMax m))
           then nd
           else let res =  
                     updPosAndBufPos
                     (r+1, let nc = endOfLine nd (r+1) in
                             if pc <= nc
                                then pc
                                else nc) nd in
                if (r+1) < (getHeight d)
                   then res
                   else setSPos (sp + 1) res
  where nd = insBufAndNew d
        pc = getHPos nd
        sp = getSPos nd

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

insTab :: Document -> Document
insTab = (f . f . f .f)
  where f = insertDoc ' '

lookupDoc :: Int -> Document -> Rope
lookupDoc i d = fromJust $ Map.lookup i $ getLines d
