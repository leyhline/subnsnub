module Lib
    ( xmlToSrt
    , Caption(..)
    ) where

import Text.Printf
import Data.Maybe (mapMaybe)
import Text.XML.Light

--               hour min  sec  ms
data Time = Time Integer Integer Integer Integer
  deriving (Eq, Ord)

instance Show Time where
  show (Time h m s ms) = printf "%02d:%02d:%02d,%03d" h m s ms

data Caption = Caption
  { counter   :: Integer
  , appear    :: Time
  , disappear :: Time
  , text      :: String
  }

instance Show Caption where
  show (Caption c a da t) = concat [show c, "\n", show a, " --> ", show da, "\n", t]

xmlToSrt :: String -> [Caption]
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
extractCaptions = foldl folder [] . filterChildrenName isP
  where
    isP :: QName -> Bool
    isP (QName "p" _ _) = True
    isP _ = False
    folder :: [Caption] -> Element -> [Caption]
    folder [] e = [mapper 1 e]
    folder cs@((Caption i _ _ _):_) e = mapper (i+1) e : cs
    mapper :: Integer -> Element -> Caption
    mapper i (Element _ _ contents _) =
      let (h1, m1) = (i-1) `divMod` 60
          (h2, m2) =  i    `divMod` 60
          t = concatMap extractText contents
          t' = if t == "" then concatMap showContent contents else t
      in Caption i (Time h1 m1 0 0) (Time h2 m2 0 0) t'

extractText :: Content -> String
extractText (Elem (Element (QName name _ _) _ contents _)) =
  let continue = concatMap extractText contents
  in case name of
    "span" -> continue
    "ruby" -> continue
    _      -> ""  -- ignores furigana
extractText (Text (CData _ t _)) = t
extractText _ = ""
