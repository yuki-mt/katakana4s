package com.yukimt.katakana
package alphabet

case object English extends AlphabetConverter{
  val vowels = Set('a', 'i', 'u', 'e', 'o')

  def decompose(str: String): Array[Sound] = {
    str.headOption.map{ c =>
      val sounds = decompose(str.tail)
      if(isHeadVowel(str)){
        if(sounds.isEmpty || sounds.head.consonant.nonEmpty)
          Sound("", c.toString) +: sounds
        else{
          val newVowel = c + sounds.head.vowel
          sounds(0) = sounds.head.copy(vowel = newVowel)
          sounds
        }
      } else {
        val newConsonant = c + sounds.head.consonant
        val isOverlapped = newConsonant.size > 1 && newConsonant.head == newConsonant(1)
        if(EnglishConsonant.consonants.contains(newConsonant) || isOverlapped){
          sounds(0) = sounds.head.copy(consonant = newConsonant)
          sounds
        } else {
          Sound(c.toString, "") +: sounds
        }
      }
    }.getOrElse(Array.empty[Sound])
  }

  def convert(sounds: Seq[Sound]): String = {
    val kVowels: Seq[Katakana] = sounds.zipWithIndex.map{
      case (sound, index) =>
        if(sound.vowel.isEmpty) ""
        else {
          val nexts: (Option[Sound], Option[Sound]) =
            if (index == sounds.size - 1) (None, None)
            else if (index == sounds.size - 2) (Some(sounds(index + 1)), None)
            else (Some(sounds(index + 1)), Some(sounds(index + 2)))

          EnglishVowel.convert(sound.consonant, sound.vowel, nexts, sounds.size)
        }
    }
    sounds.zipWithIndex.map{
      case (sound, index) =>
        val isLast = index == sounds.size - 1
        if(sound.consonant.isEmpty)
          kVowels(index)
        else {
          EnglishConsonant.consonants.get(sound.consonant).getOrElse(sound.consonant.head.toString) match {
            case c: EnglishConsonant.Normal =>
              c.getKatakana(kVowels(index))
            
            case c: EnglishConsonant.Sokuon =>
              val beforeVowel =
                if(index == 0) ""
                else kVowels(index - 1)
              c.getKatakana(kVowels(index), beforeVowel, isLast)
            
            case EnglishConsonant.G =>
              EnglishConsonant.G.getKatakana(kVowels(index), sound.vowel.nonEmpty, isLast)
            
            case EnglishConsonant.M =>
              EnglishConsonant.M.getKatakana(kVowels(index), isLast)
            
            case EnglishConsonant.S =>
              val beforeConsonant =
                if(index == 0) ""
                else sounds(index - 1).consonant
              EnglishConsonant.S.getKatakana(kVowels(index), isLast, beforeConsonant)
            
            case EnglishConsonant.C =>
              EnglishConsonant.C.getKatakana(kVowels(index), sound.vowel)
          }
        }
    }.mkString
  }

  def isHeadVowel(str: Alphabet) = {
    str.headOption.exists(_ match {
      case c if vowels contains c =>
        true
      case 'y' =>
        !(str.length > 1 && (str(1) == 'a' || str(1) == 'u' || str(1) == 'o'))
      case 'w' =>
        !(str.length > 1 && (vowels.contains(str(1)) || str(1) == 'y' || str(1) == 'h'))
      case 'r' =>
        !(str.length > 1 && (vowels.contains(str(1)) || str(1) == 'y' || str(1) == 'r'))
      case _ => false
    })
  }
}
