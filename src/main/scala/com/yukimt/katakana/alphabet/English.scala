package com.yukimt.katakana
package alphabet

case object English extends AlphabetConverter{
  val vowels = Set('a', 'i', 'u', 'e', 'o')

  def decompose(str: String): Array[Sound] = {
    str.headOption.map{ c =>
      val sounds = decompose(str.tail)
      if(isHeadVowel(str)){
        if(sounds.isEmpty || sounds.head.consonant.isDefined)
          Sound(None, Some(c.toString)) +: sounds
        else{
          val newVowel = sounds.head.vowel.map(v => c + v).getOrElse(c.toString)
          sounds(0) = sounds.head.copy(vowel = Some(newVowel))
          sounds
        }
      } else {
        val newConsonant = sounds.head.consonant.map(con => c + con).getOrElse(c.toString)
        val isOverlapped = newConsonant.size > 1 && newConsonant.head == newConsonant(1)
        if(EnglishConsonant.consonants.contains(newConsonant) || isOverlapped){
          sounds(0) = sounds.head.copy(consonant = Some(newConsonant))
          sounds
        } else {
          Sound(Some(c.toString), None) +: sounds
        }
      }
    }.getOrElse(Array.empty[Sound])
  }

  def convert(sounds: Seq[Sound]): String = {
    val kVowels: Seq[Katakana] = sounds.zipWithIndex.map{
      case (sound, index) =>
        sound.vowel.map{v =>
          val nexts: (Option[Sound], Option[Sound]) =
            if (index == sounds.size - 1) (None, None)
            else if (index == sounds.size - 2) (Some(sounds(index + 1)), None)
            else (Some(sounds(index + 1)), Some(sounds(index + 2)))

          EnglishVowel.convert(sound.consonant, v, nexts, sounds.size)
        }.getOrElse("")
    }
    sounds.zipWithIndex.map{
      case (sound, index) =>
        val isLast = index == sounds.size - 1
        sound.consonant.map{ c =>
          EnglishConsonant.consonants.get(c).getOrElse(c.head.toString) match {
            case c: EnglishConsonant.Normal =>
              c.getKatakana(kVowels(index))
            
            case c: EnglishConsonant.Sokuon =>
              val beforeVowel =
                if(index == 0) ""
                else kVowels(index - 1)
              c.getKatakana(kVowels(index), beforeVowel, isLast)
            
            case EnglishConsonant.G =>
              EnglishConsonant.G.getKatakana(kVowels(index), sound.vowel.isDefined, isLast)
            
            case EnglishConsonant.M =>
              EnglishConsonant.M.getKatakana(kVowels(index), isLast)
            
            case EnglishConsonant.S =>
              val beforeConsonant =
                if(index == 0) ""
                else sounds(index - 1).consonant.getOrElse("")
              EnglishConsonant.S.getKatakana(kVowels(index), isLast, beforeConsonant)
            
            case EnglishConsonant.C =>
              EnglishConsonant.C.getKatakana(kVowels(index), sound.vowel.getOrElse(""))
          }
        }.getOrElse(kVowels(index)) 
    }.mkString
  }

  def isHeadVowel(str: String) = {
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
