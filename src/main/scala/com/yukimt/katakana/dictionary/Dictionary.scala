package com.yukimt.katakana.dictionary

/**
 * Able to convert strings to Katakana by using dictionary information.
 * Initially, read dictionary information from text files,
 * and save the information as Index
 *
 * ************Dictionary File Format*******************
 * "word,converted katakana" for each line
 * e.g.
 * player,プレイヤー
 * shirt,シャツ
 * 夜露死苦,ヨロシク
 * *****************************************************
 */
abstract class Dictionary(userDictionaryPath: Option[String] = None){
  /**
   * read dictionary information from text files
   */
  def readDictionary: Unit
  
  /**
   * used in readDictionary method
   * check to see if the format of the dictionary is valid
   */
  protected def validateDictionary(line: String): Boolean

  /**
   * used in readDictionary method
   * build index from each line of dictionary information
   */
  protected def buildIndex(lines: Seq[String]): Map[Char, Pronunciation]
  
  /**
   * used in readDictionary method
   * save index to some data source (e.g. Redis, memcache, ...)
   */
  protected def save(index: Map[Char, Pronunciation]): Boolean
  
  readDictionary



  /**
   * convert word into Katakana based on the index 
   */
  def convert(word: String): String
}
