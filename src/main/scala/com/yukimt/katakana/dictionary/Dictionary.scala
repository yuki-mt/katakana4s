package com.yukimt.katakana.dictionary

import scala.io.Source
import java.util.UUID
/**
 * Able to convert strings to Katakana by using dictionary information.
 * Initially, read dictionary information from text files,
 * and save the information as Index
 *
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
  val dicFilePath = getClass.getResource("/katakana.dic").toURI.toString
  
  //read dictionary information from text file
  def readDictionary(filePath: String) = {
    // convert each line of the file into Index
    val indices = Source.fromFile(dicFilePath).getLines.map{ line =>
      val (term, reading) = splitLine(line)
      val letter = getLetter(term)
      Index(letter, term, reading)
    }

    //save 1000 liens at one time to prevent OutOfMemorryError
    indices.grouped(1000).map{ indexGroup =>
      save(indexGroup.toList.groupBy(_.letter).mapValues(_.map(index => index.term -> index.reading).toMap))
    }
  }

  //split line to Term and Reading
  protected def splitLine(line: String): (Term, Reading) = {
    val uuid = UUID.randomUUID.toString
    //split by ',', but not split by '\,'
    val splitted = line.replaceAll("\\\\,", uuid).split(",").map(_.replaceAll(uuid, ","))

    if(splitted.length != 2) throw new RuntimeException(s"The format of the dictionary is wrong.\n WrongLine: $line")
    val Array(term, reading) = splitted

    // Term needs to be lower-case
    (term.toLowerCase, reading)
  }

  protected def getLetter(term: Term): Char = {
    //in the case of Regular Expression
    if(term.length > 1 && term.head == '/' && term.last == '/'){
      // if regular expression is formed with /^.../, regard the third letter as the first letter
      if(term.length > 3 && term(1) == '^'){
        if(term(2).toString.matches("[a-z]")) term(2)
        else OtherLetter
      }
      else RELetter
    } else {
      if(term.head.toString.matches("[a-z]")) term.head
      else OtherLetter
    }
  }
  
  //save index to some data source (e.g. Redis, memcache, ...)
  protected def save(index: Map[Char, Map[Term, Reading]]): Unit
  
  readDictionary(dicFilePath)
  userDictionaryPath.foreach(u => readDictionary(u))



  /**
   * convert word into Katakana based on the index 
   */
  def convert(word: String): String
}
