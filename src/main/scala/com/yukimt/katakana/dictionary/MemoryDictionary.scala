package com.yukimt.katakana.dictionary

import scala.io.Source
import java.net.URI
class MemoryDictionary(userDictionaryPath: Option[String] = None) extends Dictionary(userDictionaryPath){
  private var re: Map[Term, Reading] = Map.empty
  private var kanji: Map[Term, Reading] = Map.empty
  private var english: Map[Char, Map[Term, Reading]] = Map.empty

  protected def getRE = re
  protected def getKanji = kanji
  protected def getEnglish(letter: Char) = english.get(letter).getOrElse(Map.empty[Term, Reading])
  
  protected def save(index: Map[Char, Map[Term, Reading]]) = {
    index.get(RELetter).foreach(r => re = re ++ r)
    index.get(KanjiLetter).foreach(k => kanji = kanji ++ k)
    english = english ++ (index - RELetter - KanjiLetter)
  }
}
