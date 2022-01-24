module XmlExtract
  ( extractParagraphs
  ) where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML.Light

extractParagraphs :: Text -> [Text]
extractParagraphs = maybe [] processHtml . find html . onlyElems . parseXML
  where html (Element (QName "html" _ _) _ _ _) = True
        html _ = False

processHtml :: Element -> [Text]
processHtml = maybe [] processDivs . filterElement isDivMain
  where isDivMain (Element (QName "div" _ _) attrs _ _) = lookupAttr (unqual "class") attrs == Just "main"
        isDivMain _ = False

processDivs :: Element -> [Text]
processDivs topE = map extractText paragraphs
  where
    paragraphs = filterChildrenName isP topE
    isP (QName "p" _ _) = True
    isP _ = False

extractText :: Element -> Text
extractText (Element _ _ contents _) =
  let text = T.concat $ map processSpan contents
      verbatim = T.concat $ map (T.pack . showContent) contents
  in if text == "" then verbatim else text

processSpan :: Content -> Text
processSpan (Elem (Element (QName name _ _) _ contents _)) =
  let continue = T.concat $ map processSpan contents
  in case name of
    "span" -> continue
    "ruby" -> continue
    _      -> ""  -- ignore furigana
processSpan (Text (CData _ t _)) = T.pack t
processSpan _ = ""
