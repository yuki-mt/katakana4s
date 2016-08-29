package com.yukimt.katakana.dictionary

import org.specs2.mutable.Specification
import org.mockito.Mockito._
import java.net.URI

class DictionarySpec extends Specification{
  val dic = new TestDictionary
  "Dictionary" should {
    "is alphabet" in {
      dic.isAlphabet('c') === true
      dic.isAlphabet('Z') === true
      dic.isAlphabet('ケ') === false
    }

    "split line" in {
      dic.splitLine("aLl,オール") === ("all", "オール")
      dic.splitLine("a\\,ll,オール") === ("a,ll", "オール")
      dic.splitLine("all,オール,a") must throwA[RuntimeException]
    }

    "get letter" in {
      dic.getLetter("/test/") === RELetter
      dic.getLetter("夜") === KanjiLetter
      dic.getLetter("abc") === 'a'
    }

    "read dictionary" in {
      val wrongDic = getClass.getResource("/wrongDic.dic").toURI
      dic.readDictionary(wrongDic) must throwA(new RuntimeException("reading of dictionary needs to be all Katakana or regular expression. Wrong Part: 夜露死苦,ヨロシク!"))

      val userDic = getClass.getResource("/userDic.dic").toURI.toString
      val spyDic = spy(new TestDictionary(Some(userDic)))
      spyDic.setup
      verify(spyDic, times(1)).save(Map('b'->Map("beer"->"ビール"), '/'->Map("^al([^aiueo])"->"オール$1"), '#'->Map("夜露死苦"->"ヨロシク")))
      verify(spyDic, times(1)).save(Map('t'->Map("together"->"トゥギャザー"), '#'->Map("夜露死苦"->"ヨロシクゥ")))
      1 === 1
    }

    "convert" in {
      val userDic = getClass.getResource("/userDic.dic").toURI.toString
      val myDic = spy(new TestDictionary(Some(userDic)))
      myDic.setup
      myDic.convert("腹痛つらい") === "ハライタつらい"
      myDic.convert("altogether") === "オールトゥギャザー"
      myDic.convert("THE") === "ザ"
      myDic.convert("they") === "they"
    }
  }
}

class TestDictionary(userDic:Option[String] = None) extends Dictionary(userDic){
  override def readDictionary(filePath: URI) = super.readDictionary(filePath)
  override def splitLine(line: String) = super.splitLine(line)
  override def getLetter(term: Term) = super.getLetter(term)
  override def isAlphabet(c: Char) = super.isAlphabet(c)
  override def getRE = Map("^al([^aiueo])"->"オール$1")
  override def getKanji = Map("腹痛"->"ハライタ")
  override def getEnglish(letter: Char) = Map("together"->"トゥギャザー", "the"->"ザ")
  override def save(index: Map[Char, Map[Term, Reading]]) = ()
}
