package com.yukimt.katakana

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import org.mockito.Mockito._
import dictionary.Dictionary
import tokenizer._
import alphabet.{Alphabet, Sound}

class ConverterSpec extends Specification with Mockito{
  var tok = mock[Tokenizer]
  var a = mock[Alphabet]
  var d = mock[Dictionary]
  
  def getConverter(hasDictionary: Boolean) = {
    tok = mock[Tokenizer]
    a = mock[Alphabet]
    d = mock[Dictionary]
    tok.tokenize(any[String]) returns {
      Seq(Token("本当", "ホントウ"), Token("football", "football"), Token("is", "is"), Token("最高", "サイコウ"))
    }
    d.convert("本当") returns "マジデ"
    d.convert("football") returns "footボール"
    d.convert("is") returns "イズ"
    d.convert("最高") returns "最高"
    a.decompose(any[String]) returns Array.empty[Sound]
    a.convert(any[Seq[Sound]]) returns "フット"

    if(hasDictionary) new TestConverter(tok, a, Some(d))
    else new TestConverter(tok, a, None)
  }

  sequential
  "Converter" should {
    "insert camel space" in {
      val con = getConverter(true)
      con.insertCamelSpace("AppStoreYeahとまとABDほげhogehoge") === "App Store YeahとまとABDほげhogehoge"
    }

    "convert alphabet" in {
      val con = getConverter(true)
      val res = con.convertAlphabet("hogeほげfugaふが")
      verify(a, times(1)).decompose("hoge")
      verify(a, times(1)).decompose("fuga")
      res === "フットほげフットふが"
    }

    "comobine readings" in {
      val con = getConverter(true)
      val readings = Seq("オオ", "super", "hyper", "アア", "ホゲ")
      val conversions = Seq("あ", "スーパー", "ハイパー", "イェイ", "お")
      con.combineReadings(readings, conversions, ConversionMode.Space) === "オオ スーパー ハイパー イェイ ホゲ"
      con.combineReadings(readings, conversions, ConversionMode.EnglishNoSpace) === "オオ スーパーハイパー イェイ ホゲ"
      con.combineReadings(readings, conversions, ConversionMode.NoSpace) === "オオスーパーハイパーイェイホゲ"
    }

    "convert" in {
      val con = getConverter(true)
      val res = con.convert("ABC NewStoreで買い物")
      verify(tok, times(1)).tokenize("ABC New Storeで買い物")
      verify(d, times(1)).convert("本当")
      verify(d, times(1)).convert("football")
      verify(d, times(1)).convert("is")
      verify(d, times(1)).convert("最高")
      verify(a, times(1)).decompose("foot")
      res === "マジデ フットボール イズ サイコウ"
      
      con.convert("", ConversionMode.NoSpace) === "マジデフットボールイズサイコウ"
      con.convert("", ConversionMode.EnglishNoSpace) === "マジデ フットボールイズ サイコウ"
    }
  }
}

class TestConverter(tokenizer: Tokenizer, alpha: Alphabet, dic: Option[Dictionary] = None) extends Converter(tokenizer, alpha, dic){
  override def insertCamelSpace(str: String) = super.insertCamelSpace(str)
  override def convertAlphabet(word: String) = super.convertAlphabet(word)
  override def combineReadings(r: Seq[String], c: Seq[String], m: ConversionMode) = super.combineReadings(r, c, m)
}
