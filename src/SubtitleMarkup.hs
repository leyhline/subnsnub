{-# LANGUAGE LambdaCase #-}

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
Module      : Markup
Description : Data type for subtitle with basic markup.

Mainly for text with simple markup tags:
<b />, <i />, <u />, <ruby />, <rt />
-}
module SubtitleMarkup
  ( Markup
  , Content(..)
  , showSub
  , readSub
  , readXml
  , toXml
  , toAnki
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.XML.Light as XML

type Markup = [Content]

data Content
  = SubText Text
  | SubBold Markup
  | SubItalic Markup
  | SubUnderline Markup
  | SubRuby Markup
  | SubRt Markup
  deriving (Show, Eq)

showSub :: Markup -> Text
showSub = T.concat . map showContent

showContent :: Content -> Text
showContent = \case
  SubText text        -> text
  SubBold markup      -> T.concat ["<b>", showSub markup, "</b>"]
  SubItalic markup    -> T.concat ["<i>", showSub markup, "</i>"]
  SubUnderline markup -> T.concat ["<u>", showSub markup, "</u>"]
  SubRuby markup      -> T.concat ["<ruby>", showSub markup, "</ruby>"]
  SubRt markup        -> T.concat ["<rt>", showSub markup, "</rt>"]

readSub :: Text -> Markup
readSub = concatMap readXml . XML.parseXML

readXml :: XML.Content -> Markup
readXml = \case
  (XML.Elem (XML.Element (XML.QName "b" _ _) _ contents _))    -> [SubBold      $ concatMap readXml contents]
  (XML.Elem (XML.Element (XML.QName "i" _ _) _ contents _))    -> [SubItalic    $ concatMap readXml contents]
  (XML.Elem (XML.Element (XML.QName "u" _ _) _ contents _))    -> [SubUnderline $ concatMap readXml contents]
  (XML.Elem (XML.Element (XML.QName "ruby" _ _) _ contents _)) -> [SubRuby      $ concatMap readXml contents]
  (XML.Elem (XML.Element (XML.QName "rt" _ _) _ contents _))   -> [SubRt        $ concatMap readXml contents]
  (XML.Elem (XML.Element (XML.QName "span" _ _) _ contents _)) -> concatMap readXml contents
  (XML.Text XML.CData { XML.cdData = text })                   -> [SubText      $ T.pack text]
  _ -> []

toXml :: Markup -> [XML.Content]
toXml = map contentToXml

contentToXml :: Content -> XML.Content
contentToXml = \case
  SubText text        -> XML.Text $ XML.CData XML.CDataText (T.unpack text) Nothing
  SubBold markup      -> XML.Elem $ XML.node (XML.unqual "b") (toXml markup)
  SubItalic markup    -> XML.Elem $ XML.node (XML.unqual "i") (toXml markup)
  SubUnderline markup -> XML.Elem $ XML.node (XML.unqual "u") (toXml markup)
  SubRuby markup      -> XML.Elem $ XML.node (XML.unqual "ruby") (toXml markup)
  SubRt markup        -> XML.Elem $ XML.node (XML.unqual "rt") (toXml markup)

toAnki :: Markup -> Text
toAnki = T.concat . map contentToAnki

contentToAnki :: Content -> Text
contentToAnki = \case
  SubText text        -> text
  SubBold markup      -> toAnki markup
  SubItalic markup    -> toAnki markup
  SubUnderline markup -> toAnki markup
  SubRuby markup      -> toAnki markup
  SubRt _             -> ""
