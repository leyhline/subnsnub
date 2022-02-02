{-
subnsnub - helps to create subtitles from and for audiobooks
Copyright (C) 2022  Thomas Leyh <thomas.leyh@mailbox.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-|
Module      : XmlExtract
Description : Extracts p element contents from ebook XML and return cleaned up.

The content of the <p /> elements (paragraphs) as Text as well as simple markup
tags (i.e. <b />, <i />, <u />, <ruby />, <rt />) are expected output.
-}
module XmlExtract
  ( extractParagraphs
  , Paragraph
  ) where

import Data.List (find)
import Data.Text (Text)
import Text.XML.Light

type Paragraph = [Content]

extractParagraphs :: Text -> [Paragraph]
extractParagraphs = maybe [] processHtml . find html . onlyElems . parseXML
  where html (Element (QName "html" _ _) _ _ _) = True
        html _ = False

processHtml :: Element -> [Paragraph]
processHtml = maybe [] processDivs . filterElement isDivMain
  where isDivMain (Element (QName "div" _ _) attrs _ _) = lookupAttr (unqual "class") attrs == Just "main"
        isDivMain _ = False

processDivs :: Element -> [Paragraph]
processDivs topE = map processP paragraphs
  where
    paragraphs = filterChildrenName isP topE
    isP (QName "p" _ _) = True
    isP _ = False

processP :: Element -> Paragraph
processP (Element _ _ contents _) =
  let paragraph = concatMap processContent contents
  in if null paragraph then contents else paragraph

processContent :: Content -> Paragraph
processContent (Elem (Element (QName name _ _) _ contents _))
  | name `elem` preserveTags = [Elem $ node (unqual name) processChildren]
  | name `elem` discardTags  = processChildren
  | otherwise                = []
  where preserveTags = ["ruby", "rt", "u", "b", "i"]
        discardTags  = ["span"]
        processChildren = concatMap processContent contents
processContent (Text cdata) = [Text cdata { cdLine = Nothing }]
processContent _ = []
