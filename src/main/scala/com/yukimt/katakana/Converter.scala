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
  val katakana = ('ァ' to 'ン')

  def convert(str: String){
    val tokens = tokenizer.tokenize(str)
    val readings = dic.map{ d => 
      val dicTerms = tokens.map(t => d.convert(t.term))
      tokens.map(_.reading).zip(dicTerms).map{
        case (reading, dicTerm) =>
          if(isKatakana(dicTerm)) dicTerm
          else reading
      }
    }.getOrElse(tokens.map(_.reading))

    readings.map(r => alpha.convert(alpha.decompose(r))).mkString(" ")
  }

  def isKatakana(str: String) = {
    str.forall(c => katakana contains c)
  }
}
