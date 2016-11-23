package com.yukimt.katakana

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import org.mockito.Mockito._
import tokenizer._
import alphabet.{AlphabetConverter, Sound}

class ConverterSpec extends Specification with Mockito{
  var tok = mock[Tokenizer]
  var a = mock[AlphabetConverter]
  
  def getConverter = {
    tok = mock[Tokenizer]
    a = mock[AlphabetConverter]
    tok.tokenize(any[String]) returns {
      Seq(Token("本当", "ホントウ"), Token("football", "football"), Token("最高", "サイコウ"))
    }
    a.decompose(any[String]) returns Array.empty[Sound]
    a.convert(any[Seq[Sound]]) returns "フットボール"

    new TestConverter(tok, a)
  }

  sequential
  "Converter" should {
    "insert camel space" in {
      val con = getConverter
      con.insertCamelSpace("AppStoreYeahとまとABDほげhogehoge") === "App Store YeahとまとABDほげhogehoge"
    }

    "convert alphabet" in {
      val con = getConverter
      val res = con.convertAlphabet("hogeほげfugaふが")
      verify(a, times(1)).decompose("hoge")
      verify(a, times(1)).decompose("fuga")
      res === "フットボールほげフットボールふが"
    }

    "comobine readings" in {
      val con = getConverter
      val readings = Seq("オオ", "super", "hyper", "アア", "ホゲ")
      val conversions = Seq("あ", "スーパー", "ハイパー", "イェイ", "お")
      con.combineReadings(readings, conversions, ConversionMode.Space) === "オオ スーパー ハイパー イェイ ホゲ"
      con.combineReadings(readings, conversions, ConversionMode.AlphabetNoSpace) === "オオ スーパーハイパー イェイ ホゲ"
      con.combineReadings(readings, conversions, ConversionMode.NoSpace) === "オオスーパーハイパーイェイホゲ"
    }

    "convert" in {
      val con = getConverter
      val res = con.convert("ABC NewStoreで買い物")
      verify(tok, times(1)).tokenize("ABC New Storeで買い物")
      verify(a, times(1)).decompose("football")
      res === "ホントウ フットボール サイコウ"
      
      con.convert("", ConversionMode.NoSpace) === "ホントウフットボールサイコウ"
      con.convert("", ConversionMode.AlphabetNoSpace) === "ホントウ フットボール サイコウ"
    }
  }
}

class TestConverter(tokenizer: Tokenizer, alpha: AlphabetConverter) extends Converter(tokenizer, alpha){
  override def insertCamelSpace(str: String) = super.insertCamelSpace(str)
  override def convertAlphabet(word: String) = super.convertAlphabet(word)
  override def combineReadings(r: Seq[String], c: Seq[String], m: ConversionMode) = super.combineReadings(r, c, m)
}
