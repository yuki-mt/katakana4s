package com.yukimt.katakana

object ConverterImplicits{
  val katakanaSet = ('ァ' to 'ン').toSet + 'ー'

  implicit class ImplicitChar(c: Char){
    def isKatakana = c => katakanaSet contains c
    def isAlphabet = c.toString.matches("[a-z|A-Z]")
  }

  implicit class ImplicitString(str: String){
    def isAllKatakana = str.forall(c => katakanaSet contains c)
    def isAllAlphabet = str.forall(_.toString.matches("[a-z|A-Z]"))
    //split alphabets and non-alphabets
    //e.g. "オールtogether昨日" -> Array("オール", "together", "昨日")
    def toComponents = {
      str.foldLeft(Array("")){(acc, c) =>
        if(acc.last.isEmpty || acc.last.last.isAlphabet == c.isAlphabet){
          acc(acc.size - 1) = acc.last + c
          acc
        }
        else
          acc :+ c.toString
      }
    }
  }
}
