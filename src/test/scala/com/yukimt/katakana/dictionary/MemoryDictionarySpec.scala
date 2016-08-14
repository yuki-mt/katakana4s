package com.yukimt.katakana.dictionary

import org.specs2.mutable.Specification
import org.mockito.Mockito._
import java.net.URI

class MemoryDictionarySpec extends Specification{
  sequential
  
  val userDic = getClass.getResource("/userDic.dic").toURI.toString
  val dic = new TestMemoryDictionary(Some(userDic))

  "MemoryDictionary" should {
    "read dictionary" in {
      dic.setup
      dic.getRE === Map("^al([^aiueo])"->"オール$1")
      dic.getKanji === Map("夜露死苦"->"ヨロシクゥ")
      dic.getEnglish('t') === Map("together"->"トゥギャザー")
      dic.getEnglish('b') === Map("beer"->"ビール")
      dic.getEnglish('a') === Map.empty
    }
  }
}

//Make protected methods public
class TestMemoryDictionary(userDic:Option[String] = None) extends MemoryDictionary(userDic){
  override def readDictionary(filePath: URI) = super.readDictionary(filePath)
  override def getRE = super.getRE
  override def getKanji = super.getKanji
  override def getEnglish(letter: Char) = super.getEnglish(letter)
}
