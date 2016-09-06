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

  def convert(str: String, mode: ConversionMode = ConversionMode.Space){
    val tokens = tokenizer.tokenize(str)
    val conversions = tokens.map{t => 
      val dicWord = dic.map(_.convert(t.term)).getOrElse(t.term)
      ConverterUtil.splitWord(dicWord).map{ w =>
        if(ConverterUtil.isAlphabet(w.head))
          alpha.convert(alpha.decompose(w))
      }.mkString
    }
    
    val readings = tokens.map(_.reading).zip(conversions).map{
      case (reading, conversion) =>
        val r = 
          if(ConverterUtil.isAllKatakana(conversion)) conversion
          else reading
        if(mode == ConversionMode.EnglishNoSpace && !ConverterUtil.isAllAlphabet(reading)) r + ' '
        else r
    }

    if(mode == ConversionMode.Space)
      readings.mkString(" ")
    else
      readings.mkString.trim
  }
}
