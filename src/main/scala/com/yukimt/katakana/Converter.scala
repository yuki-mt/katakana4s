package com.yukimt.katakana

import dictionary.Dictionary
import tokenizer.Tokenizer
import alphabet.Alphabet

/**
 * convert any string into Katakana by using "convert" method
 * (insert space between each term)
 * e.g. "昨日I am Legendという映画を観た" will be
 * "キノウ アイ アム レジェンド トイウ エイガ ヲ ミタ"
 *
 * if you do not want to use dictionary to convert strings, leave "dic" None
 */
class Converter(tokenizer: Tokenizer, alpha: Alphabet, dic: Option[Dictionary] = None){
  //reading dicrionary information
  dic.foreach(_.setup)

  def convert(str: String, mode: ConversionMode = ConversionMode.Space) = {
    val tokens = tokenizer.tokenize(insertCamelSpace(str))

    val conversions = tokens.map{t => 
      val dicWord = dic.map(_.convert(t.term)).getOrElse(t.term)
      convertAlphabet(dicWord)
    }
    
    combineReadings(tokens.map(_.reading), conversions, mode)
  }

  //insert space between English term for camel case
  protected def insertCamelSpace(str: String) = str.foldLeft(""){(acc, c) =>
    if(acc.nonEmpty && Character.isLowerCase(acc.last) && Character.isUpperCase(c))
      acc + ' ' + c
    else
      acc + c
  }

  //call alpha.convert for only alphabet part
  protected def convertAlphabet(word: String) = ConverterUtil.splitWord(word).map{ w =>
    if(ConverterUtil.isAlphabet(w.head)) alpha.convert(alpha.decompose(w))
    else w
  }.mkString

  protected def combineReadings(readingTokens: Seq[String], conversions: Seq[String], mode: ConversionMode) = readingTokens.zip(conversions).foldLeft(""){(acc, z) =>
    val readingToken = z._1
    val conversion = z._2
    val reading = 
      if(ConverterUtil.isAllKatakana(conversion)) conversion
      else readingToken

    mode match {
      case ConversionMode.Space =>
        if(acc.isEmpty) reading else acc + ' ' + reading
      case ConversionMode.EnglishNoSpace =>
        if(ConverterUtil.isAlphabet(readingToken.head))
          acc + reading
        else{
          if(acc.lastOption.getOrElse('a') == ' ')
            acc + reading + ' '
          else
            acc + ' ' + reading + ' '
        }
      case ConversionMode.NoSpace => acc + reading
    }
  }.trim
}
