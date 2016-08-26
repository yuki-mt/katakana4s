package com.yukimt.katakana.alphabet

object English extends Alphabet{
  val vowels = Seq('a', 'i', 'u', 'e', 'o')

  def decompose(str: String): Seq[Sound] = Seq.empty

  def convert(sounds: Seq[Sound]): String = ""
}
