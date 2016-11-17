package com.yukimt.katakana
package alphabet

case object English extends AlphabetConverter{
  val vowels = Set('a', 'i', 'u', 'e', 'o')

  def decompose(str: String): Array[Sound] = {
    val res = _decompose(str)
    if (res.size > 1 && res.last == Sound("r", "e")) {
      val temp = res(res.size - 2)
      res(res.size - 2) = temp.copy(vowel = temp.vowel + "re")
      res.dropRight(1)
    } else res
  }
  def _decompose(str: String): Array[Sound] = {
    str.headOption.map{ c =>
      val sounds = _decompose(str.tail)
      if(isHeadVowel(str)){
        if(sounds.isEmpty || sounds.head.consonant.nonEmpty)
          Sound("", c.toString) +: sounds
        else{
          val newVowel = c + sounds.head.vowel
          sounds(0) = sounds.head.copy(vowel = newVowel)
          sounds
        }
      } else {
        val newConsonant = c + sounds.headOption.map(_.consonant).getOrElse("")
        val isOverlapped = newConsonant.size > 1 && newConsonant.head == newConsonant(1)
        if(sounds.nonEmpty && EnglishConsonant.consonants.contains(newConsonant) || isOverlapped){
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
          val nexts: (Option[Sound], Option[Sound], Option[Sound]) =
            if (index == sounds.size - 1) (None, None, None)
            else if (index == sounds.size - 2) (Some(sounds(index + 1)), None, None)
            else if (index == sounds.size - 3) (Some(sounds(index + 1)), Some(sounds(index + 2)), None)
            else (Some(sounds(index + 1)), Some(sounds(index + 2)), Some(sounds(index + 3)))

          val isFirst = index == 0
          EnglishVowel.convert(sound.consonant, sound.vowel, nexts, sounds.size, isFirst)
        }
    }
    sounds.zipWithIndex.map{
      case (sound, index) =>
        val isLast = index == sounds.size - 1 || index == sounds.size - 2 && (sounds(index+1) == Sound("l", "y") || sounds(index+1) == Sound("s", ""))
        val isNextLast = index == sounds.size - 2 || index == sounds.size - 3 && (sounds(index+2) == Sound("l", "y") || sounds(index+2) == Sound("s", ""))

        val beforeKVowel =
          if(index == 0) None
          else Some(kVowels(index - 1))

        val beforeSound =
          if(index == 0) None
          else Some(sounds(index - 1))

        val nextSound =
          if(isLast) Sound("", "")
          else sounds(index + 1)

        val isOverlapped = sound.consonant.size > 1 && sound.consonant.distinct.size == 1
        
        if(sound.consonant.isEmpty){
          kVowels(index)
        
        } else if (isNextLast && sound == Sound("m", "") && (sounds(index+1) == Sound("n", "") || sounds(index+1) == Sound("b", ""))){
          "ム"
        } else if (isNextLast && sound == Sound("g", "") && sounds(index+1) == Sound("n", "")){
          "ン"
        } else if (isLast && sounds.size > 1 && sound == Sound("n", "") && index > 0 && sounds(index-1) == Sound("g", "")){
          ""
        } else if (isLast && sounds.size > 1 && index > 0 && sounds(index-1) == Sound("m", "") && (sound == Sound("n", "") || sound == Sound("b", ""))){
          ""
        } else if (sound == Sound("l", "") && beforeSound.exists(_.vowel == "a") && nextSound.consonant == "k"){
          ""
        } else {
          EnglishConsonant.consonants.get(sound.consonant)
            .getOrElse(EnglishConsonant.consonants(sound.consonant.head.toString)) match {
            case c: EnglishConsonant.Normal =>
              c.getKatakana(kVowels(index))
            
            case c: EnglishConsonant.Sokuon =>
              c.getKatakana(kVowels(index), beforeKVowel, isLast, isOverlapped)
            
            case EnglishConsonant.T =>
              EnglishConsonant.T.getKatakana(kVowels(index), sound.vowel, beforeKVowel, isLast)
            
            case EnglishConsonant.G =>
              EnglishConsonant.G.getKatakana(kVowels(index), sound.vowel, beforeKVowel, isLast, index == 0)
            
            case EnglishConsonant.M =>
              EnglishConsonant.M.getKatakana(kVowels(index), isLast)
            
            case EnglishConsonant.S =>
              EnglishConsonant.S.getKatakana(kVowels(index), sound.vowel, beforeSound, isOverlapped, isLast)
            
            case EnglishConsonant.C =>
              EnglishConsonant.C.getKatakana(kVowels(index), sound.vowel)
            
            case EnglishConsonant.Gh =>
              EnglishConsonant.Gh.getKatakana(kVowels(index), sound.vowel, beforeSound.map(_.vowel), isLast)
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
