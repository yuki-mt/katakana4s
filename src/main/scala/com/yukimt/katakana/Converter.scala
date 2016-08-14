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
  val katakanaSet = ('ァ' to 'ン').toSet

  def convert(str: String){
    val tokens = tokenizer.tokenize(str)
    val readings = dic.map{ d => 
      val conversions = tokens.map{t => 
        val dicWord = d.convert(t.term)
        alpha.convert(alpha.decompose(dicWord))
      }
      tokens.map(_.reading).zip(conversions).map{
        case (reading, conversion) =>
          if(isKatakana(conversion)) conversion
          else reading
      }
    }.getOrElse(tokens.map(_.reading))

    readings.mkString(" ")
  }

  def isKatakana(str: String) = {
    str.forall(c => katakanaSet contains c)
  }
}
