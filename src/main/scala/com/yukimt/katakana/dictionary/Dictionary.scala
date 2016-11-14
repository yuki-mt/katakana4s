package com.yukimt.katakana.dictionary

import scala.io.Source
import java.net.URI
import com.yukimt.katakana.ConverterImplicits._
/**
 * Able to convert strings to Katakana by using dictionary information.
 * Initially, read dictionary information from text files,
 * and save the information as Index
 *
 * "word" in dictionary is case-insensitive
 * regular expression is usable in dictionary
 * regular expression is enclosed by "/"
 * if you want to use "," in the left part, escape with "\" such as a\,b,エーカンマビー
 * ************Dictionary File Format*******************
 * "word,converted katakana" for each line
 * e.g.
 * player,プレイヤー
 * shirt,シャツ
 * 夜露死苦,ヨロシク
 * /.+ball$/,ボール
 * *****************************************************
 */
abstract class Dictionary(userDictionaryPath: Option[String] = None){
  val dicFilePath = getClass.getResource("/katakana.dic").toURI
  val unUsedStr = "91qa<xsw2"
  val reSet = katakanaSet ++ (('1' to '9') :+ '$').toSet
  val rt = Runtime.getRuntime

  //read dictionary information from text file
  protected def readDictionary(filePath: URI) = {
    // convert each line of the file into Index
    val indices = Source.fromFile(filePath).getLines.map{ line =>
      val (tmpTerm, reading) = splitLine(line)
      val letter = getLetter(tmpTerm)
      val ex = new RuntimeException(s"reading of dictionary needs to be all Katakana or regular expression. Wrong Part: $line")
      val term =
        if(letter == RELetter){
          if(!reading.forall(r => reSet contains r)) throw ex
          tmpTerm.drop(1).dropRight(1)
        } else {
          if(!reading.forall(r => reSet contains r)) throw ex
          tmpTerm
        }
      Index(letter, term, reading)
    }

    //save 5000 lines at one time to prevent OutOfMemorryError
    indices.grouped(5000).foreach{ indexGroup =>
      save(indexGroup.toList.groupBy(_.letter).mapValues(_.map(index => index.term -> index.reading).toMap))
      rt.gc()
    }
  }

  //split line to Term and Reading
  protected def splitLine(line: String): (Term, Reading) = {
    //split by ',', but not split by '\,'
    val splitted = line.replaceAll("\\\\,", unUsedStr).split(",")
      .map(_.replaceAll(unUsedStr, ","))

    if(splitted.length != 2) throw new RuntimeException(s"The format of the dictionary is wrong.\n WrongLine: $line")
    val Array(term, reading) = splitted

    // Term needs to be lower-case
    (term.toLowerCase, reading)
  }

  protected def getLetter(term: Term): Char = {
    //in the case of Regular Expression
    if(term.length > 1 && term.head == '/' && term.last == '/'){
      RELetter
    } else {
      if(term.head.isAlphabet) term.head
      else KanjiLetter
    }
  }
  
  /**
   * convert word into Katakana based on the index 
   */
  def convert(word: String) = {
    if(word.head.isAlphabet){
      getEnglish(word.head).get(word).getOrElse(
        getRE.foldLeft(word.toLowerCase){(w, r) =>
          w.replaceAll(r._1, r._2)
        }.toComponents.map{ w =>
          if(w.head.isAlphabet) getEnglish(w.head).get(w).getOrElse(w)
          else w
        }.mkString
      )
    } else {
      getKanji.foldLeft(word){(w, k) =>
        w.replaceAll(k._1, k._2)
      }
    }
  }

  //get Regular Expressions from saved dictionary information
  protected def getRE: Map[Term, Reading]
  //get Kanji from saved dictionary information
  protected def getKanji: Map[Term, Reading]
  //get English from saved dictionary information
  protected def getEnglish(letter: Char): Map[Term, Reading]
  
  //save index to some data source (e.g. Redis, memcache, ...)
  protected def save(index: Map[Char, Map[Term, Reading]]): Unit
 
  def setup = {
    //initially read default and user dictionary
    readDictionary(dicFilePath)
    userDictionaryPath.foreach(u => readDictionary(new URI(u)))
  }
}
