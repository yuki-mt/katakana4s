package com.yukimt.katakana.alphabet

object Roman extends Alphabet{
  val vowels = Seq('a', 'i', 'u', 'e', 'o')

  def decompose(str: String): Seq[Sound] = {
    Seq.empty
//    str.foldLeft((Seq.empty[String], "")){(words, c) =>
//      if (vowels contains c)
//        (words._1 :+ (words._2 + c), "")
//      else
//        (words._1, words._2 + c)
//    }._1
  }

  def convert(sounds: Seq[Sound]): String = ""
}
