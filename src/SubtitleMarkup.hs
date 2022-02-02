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
Module      : SubtitleMarkup
Description : Data type for subtitle with basic markup.

Mainly for text with simple markup tags:
<b />, <i />, <u />, <ruby />, <rt />
-}
module SubtitleMarkup
  ( SubtitleMarkup
  , SubtitleContent(..)
  , showSubtitleMarkup
  ) where

import Data.Text (Text)
import qualified Data.Text as T

type SubtitleMarkup = [SubtitleContent]

data SubtitleContent = SubText Text
  | SubBold SubtitleMarkup
  | SubItalic SubtitleMarkup
  | SubUnderline SubtitleMarkup
  | SubRuby SubtitleMarkup
  | SubRt SubtitleMarkup
  deriving (Show, Eq)

showSubtitleMarkup :: SubtitleMarkup -> Text
showSubtitleMarkup = T.concat . map showSubtitleContent

showSubtitleContent :: SubtitleContent -> Text
showSubtitleContent = \case
  SubText text -> text
  SubBold markup -> T.concat ["<b>", showSubtitleMarkup markup, "</b>"]
  SubItalic markup -> T.concat ["<i>", showSubtitleMarkup markup, "</i>"]
  SubUnderline markup -> T.concat ["<u>", showSubtitleMarkup markup, "</u>"]
  SubRuby markup -> T.concat ["<ruby>", showSubtitleMarkup markup, "</ruby>"]
  SubRt markup -> T.concat ["<rt>", showSubtitleMarkup markup, "</rt>"]
