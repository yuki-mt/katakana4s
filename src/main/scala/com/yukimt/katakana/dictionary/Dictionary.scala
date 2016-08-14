package com.yukimt.katakana.dictionary

import scala.io.Source
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
  val dicFilePath = getClass.getResource("/katakana.dic").toURI.toString
  val unUsedStr = ";:+91qaxsw2"
  val katakanaSet = ('ァ' to 'ン').toSet


  //read dictionary information from text file
  def readDictionary(filePath: String) = {
    // convert each line of the file into Index
    val indices = Source.fromFile(filePath).getLines.map{ line =>
      val (term, reading) = splitLine(line)
      if(!reading.forall(r => katakanaSet contains r))
        throw new RuntimeException("reading of dictionary needs to be all Katakana")
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
      // if regular expression is formed with /^.../, regard the third letter as the first letter
      if(term.length > 3 && term(1) == '^'){
        if(isAlphabet(term(2))) term(2)
        else KanjiLetter
      }
      else RELetter
    } else {
      if(isAlphabet(term.head)) term.head
      else KanjiLetter
    }
  }
  
 
  //initially read default and user dictionary
  readDictionary(dicFilePath)
  userDictionaryPath.foreach(u => readDictionary(u))


  /**
   * convert word into Katakana based on the index 
   */
  def convert(word: String) = {
    if(isAlphabet(word.head)){
      val replacedWord:String = getRE.foldLeft(word.toLowerCase){(w, i) =>
        w.replaceAll(i.term, i.reading)
      }
      val splittedWord = replacedWord.foldLeft(Array("")){(acc, c) => 
        if(isAlphabet(acc.last.last) == isAlphabet(c))
          acc.dropRight(1) :+ (acc.last + c)
        else
          acc :+ c.toString
      }
      splittedWord.map{ w =>
        if(isAlphabet(w.head)) getEnglish(w.head).get(w).getOrElse(w)
        else w
      }.mkString
    } else {
      getKanji.foldLeft(word){(w, i) =>
        w.replaceAll(i.term, i.reading)
      }
    }
  }

  protected def isAlphabet(c: Char) = c.toString.toLowerCase.matches("[a-z]")

  //get Regular Expressions from saved dictionary information
  protected def getRE: Seq[Index]
  //get Kanji from saved dictionary information
  protected def getKanji: Seq[Index]
  //get English from saved dictionary information
  protected def getEnglish(letter: Char): Map[Term, Reading]
  
  //save index to some data source (e.g. Redis, memcache, ...)
  protected def save(index: Map[Char, Map[Term, Reading]]): Unit
}
