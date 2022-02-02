module XmlExtractSpec
  ( spec
  ) where

import XmlExtract
import Data.Text (Text)
import SubtitleMarkup
import Test.Hspec

xmlInput :: Text
xmlInput = "\
\<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html><html xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:epub=\"http://www.idpf.org/2007/ops\" xml:lang=\"ja\" class=\"vrtl\">\n\
\<head>\n\
\<meta charset=\"UTF-8\"/>\n\
\<title>この素晴らしい世界に祝福を！　あぁ、駄女神さま</title>\n\
\</head>\n\
\<body class=\"p-text\">\n\
\<div class=\"main\">\n\
\<p><br/></p>\n\
\<p><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.2.1\">「</span><ruby><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.3.1\">佐</span><rt><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.4.1\">さ</span></rt><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.5.1\">藤</span><rt><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.6.1\">とう</span></rt><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.7.1\">和</span><rt><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.8.1\">かず</span></rt><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.9.1\">真</span><rt><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.10.1\">ま</span></rt></ruby><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.11.1\">さん、ようこそ死後の世界へ。あなたはつい先ほど、不幸にも</span><ruby><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.12.1\">亡</span><rt><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.13.1\">な</span></rt></ruby><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.14.1\">くなりました。短い人生でしたが、あなたの生は終わってしまったのです」</span></p>\n\
\<p><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.15.1\">　真っ白な部屋の中、俺は</span><ruby><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.16.1\">唐</span><rt><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.17.1\">とう</span></rt><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.18.1\">突</span><rt><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.19.1\">とつ</span></rt></ruby><span xmlns=\"http://www.w3.org/1999/xhtml\" class=\"koboSpan\" id=\"kobo.20.1\">にそんな事を告げられた。</span></p>\n\
\</div>\n\
\</body>\n\
\</html>"

spec :: Spec
spec = do
  describe "extractParagraphs" $ do
    it "returns list of paragraphs from XML p elements" $
      let
        p1 = "<br />"
        p2 = "「<ruby>佐<rt>さ</rt>藤<rt>とう</rt>和<rt>かず</rt>真<rt>ま</rt></ruby>さん、ようこそ死後の世界へ。あなたはつい先ほど、不幸にも<ruby>亡<rt>な</rt></ruby>くなりました。短い人生でしたが、あなたの生は終わってしまったのです」"
        p3 = "　真っ白な部屋の中、俺は<ruby>唐<rt>とう</rt>突<rt>とつ</rt></ruby>にそんな事を告げられた。"
        expected = [p1, p2, p3]
        extracted = map showSubtitleMarkup (extractParagraphs xmlInput)
      in extracted `shouldBe` expected
