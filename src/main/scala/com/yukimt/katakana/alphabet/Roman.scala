package com.yukimt.katakana.alphabet

object Roman extends Alphabet{
  val vowels = Set('a', 'i', 'u', 'e', 'o')

  def decompose(str: String): Array[Sound] = {
    str.headOption.map{ c =>
      if(vowels contains c)
        Sound(None, Some(c.toString)) +: decompose(str.tail)
      else{
        val sounds = decompose(str.tail)
        if(sounds.isEmpty)
          throw new IllegalArgumentException(s"Roman word needs to finish vowel: $str")
        val consonant = c + sounds.head.consonant.getOrElse("")
        sounds(0) = sounds.head.copy(consonant = Some(consonant))
        sounds
      }
    }.getOrElse(Array.empty[Sound])
  }

  def convert(sounds: Seq[Sound]): String = ""
}
