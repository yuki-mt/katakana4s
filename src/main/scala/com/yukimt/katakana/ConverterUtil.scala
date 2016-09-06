package com.yukimt.katakana

object ConverterUtil{
  val katakanaSet = ('ァ' to 'ン').toSet + 'ー'

  def isKatakana(str: String) = str.forall(c => katakanaSet contains c)
  
  def isAlphabet(str: String) = str.forall(_.toString.matches("[a-z|A-Z]"))

  //split alphabets and non-alphabets
  //e.g. "オールtogether昨日" -> Array("オール", "together", "昨日")
  def splitWord(str: String) = {
    str.foldLeft(Array("")){(acc, c) =>
      if(acc.last.isEmpty || isAlphabet(acc.last.last.toString) == isAlphabet(c.toString)){
        acc(acc.size - 1) = acc.last + c
        acc
      }
      else
        acc :+ c.toString
    }
  }
}
