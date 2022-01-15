module Subtitle
  ( xmlToSrt
  , Caption(..)
  , Time(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Text.XML.Light
import Text.Printf

data Caption = Caption
  { counter   :: Integer
  , appear    :: Time
  , disappear :: Time
  , text      :: Text
  }
  deriving Eq

instance Show Caption where
  show (Caption c a da t) = concat [show c, "\n", show a, " --> ", show da, "\n", T.unpack t]

--               hour min  sec  ms
data Time = Time Integer Integer Integer Integer
  deriving (Eq, Ord)

instance Show Time where
  show (Time h m s ms) = printf "%02d:%02d:%02d,%03d" h m s ms

xmlToSrt :: Text -> [Caption]
xmlToSrt = concatMap xmlElementToSrt . mapMaybe filterHtmlElement . parseXML
  where
    filterHtmlElement :: (Content -> Maybe Element)
    filterHtmlElement (Elem e) = if qName (elName e) == "html" then Just e else Nothing
    filterHtmlElement _ = Nothing

xmlElementToSrt :: Element -> [Caption]
xmlElementToSrt = maybe [] extractCaptions . filterElement isDivMain
  where
    isDivMain (Element (QName "div" _ _) attrs _ _) = lookupAttr (unqual "class") attrs == Just "main"
    isDivMain _ = False

extractCaptions :: Element -> [Caption]
extractCaptions topElem = foldr foldToCaptions [] paragraphs
  where
    paragraphs = filterChildrenName isP topElem
    isP (QName "p" _ _) = True
    isP _ = False
    foldToCaptions :: Element -> [Caption] -> [Caption]
    foldToCaptions e [] = [makeCaption (toInteger $ length paragraphs) e]
    foldToCaptions e cs@((Caption i _ _ _):_) = makeCaption (i-1) e : cs
    makeCaption :: Integer -> Element -> Caption
    makeCaption i (Element _ _ contents _) =
      let (h1, m1) = (i-1) `divMod` 60
          (h2, m2) =  i    `divMod` 60
          t = extractText contents
          t' = if t == "" then T.concat $ map (T.pack . showContent) contents else t
      in Caption i (Time h1 m1 0 0) (Time h2 m2 0 0) t'

extractText :: [Content] -> Text
extractText = T.concat . map extractText'

extractText' :: Content -> Text
extractText' (Elem (Element (QName name _ _) _ contents _)) =
  let continue = extractText contents
  in case name of
    "span" -> continue
    "ruby" -> continue
    _      -> ""  -- ignores furigana
extractText' (Text (CData _ t _)) = T.pack t
extractText' _ = ""
