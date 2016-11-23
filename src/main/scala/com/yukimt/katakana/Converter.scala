package com.yukimt.katakana

import tokenizer.{Tokenizer, Kuromoji}
import alphabet.{AlphabetConverter, Roman}
import ConverterImplicits._

class Converter(tokenizer: Tokenizer = Kuromoji, alpha: AlphabetConverter = Roman()){
  def convert(str: String, mode: ConversionMode = ConversionMode.Space) = {
    val tokens = tokenizer.tokenize(insertCamelSpace(str))

    val conversions = tokens.map{t =>
      //convert by using alpha
      convertAlphabet(t.term)
    }

    combineReadings(tokens.map(_.reading), conversions, mode)
  }

  //insert space between English term for camel case
  //e.g. "YouAreGenius" -> "You Are Genius"
  protected def insertCamelSpace(str: String) = str.foldLeft(""){(acc, c) =>
    if(acc.nonEmpty && Character.isLowerCase(acc.last) && Character.isUpperCase(c))
      acc + ' ' + c
    else
      acc + c
  }

  //call alpha.convert for only alphabet part
  protected def convertAlphabet(word: String) = word.toComponents.map{ w =>
    if(w.head.isAlphabet) alpha.convert(alpha.decompose(w.toLowerCase))
    else w
  }.mkString

  protected def combineReadings(readingTokens: Seq[String], conversions: Seq[String], mode: ConversionMode) = readingTokens.zip(conversions).foldLeft(""){(acc, z) =>
    val readingToken = z._1
    val conversion = z._2
    val reading = 
      if(conversion.isAllKatakana) conversion
      else readingToken

    mode match {
      case ConversionMode.Space =>
        if(acc.isEmpty) reading else acc + ' ' + reading
      case ConversionMode.AlphabetNoSpace =>
        if(readingToken.head.isAlphabet)
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
