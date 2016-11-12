package com.yukimt.katakana
package alphabet

case object English extends AlphabetConverter{
  val vowels = Set('a', 'i', 'u', 'e', 'o')

  val consonants = Map(
    "b" -> EnglishConsonant.Normal(Seq("バ", "ビ", "ブ", "べ", "ボ", "ビュ"), false),
    "c" -> Seq("カ", "キ", "ク", "ケ", "コ"),
    "d" -> EnglishConsonant.Normal(Seq("ダ", "ディ", "デュ", "デ", "ド", "デュ"), true),
    "f" -> EnglishConsonant.Normal(Seq("ファ", "フィ", "フ", "フェ", "フォ", "フュ"), false),
    "g" -> Seq("ガ", "ギ", "グ", "ゲ", "ゴ"),
    "h" -> EnglishConsonant.Normal(Seq("ハ", "ヒ", "フ", "ヘ", "ホ", "ヒュ"), false),
    "j" -> EnglishConsonant.Normal(Seq("ジャ", "ジ", "ジュ", "ジェ", "ジョ", "ジュ"), false),
    "k" -> EnglishConsonant.Normal(Seq("カ", "キ", "ク", "ケ", "コ", "キュ"), true),
    "l" -> Seq("ラ", "リ", "ル", "レ", "ロ"),
    "m" -> Seq("マ", "ミ", "ム", "メ", "モ"),
    "n" -> Seq("ナ", "ニ", "ヌ", "ネ", "ノ"),
    "p" -> Seq("パ", "ピ", "プ", "ペ", "ポ"),
    "q" -> Seq("ク", "キ", "ク", "クェ", "クォ"),
    "r" -> Seq("ラ", "リ", "ル", "レ", "ロ"),
    "s" -> Seq("サ", "シ", "ス", "セ", "ソ"),
    "t" -> Seq("タ", "ティ", "トゥ", "テ", "ト"),
    "v" -> Seq("ヴァ", "ヴィ", "ヴ", "ヴェ", "ヴォ"),
    "w" -> Seq("ワ", "ウィ", "ウ", "ウェ", "ウォ"),
    "x" -> Seq("クザ", "クジ", "クズ", "クゼ", "クゾ"),
    "y" -> Seq("ヤ", "イ", "ユ", "イェ", "ヨ"),
    "z" -> Seq("ザ", "ジ", "ズ", "ゼ", "ゾ"),
    "ch" -> Seq("チャ", "チ", "チュ", "チェ", "チョ"),
    "ck" -> Seq("カ", "キ", "ク", "ケ", "コ"),
    "sh" -> Seq("シャ", "シ", "シュ", "シェ", "ショ"),
    "gh" -> Seq("", "", "フ", "", ""),
    "ph" -> Seq("ファ", "フィ", "フ", "フェ", "フォ"),
    "ps" -> Seq("サ", "シ", "ス", "セ", "ソ"),
    "ght" -> Seq("タ", "ティ", "トゥ", "テ", "ト"),
    "th" -> Seq("サ", "シ", "ス", "セ", "ソ"),
    "wh" -> Seq("ワ", "ウィ", "フ", "ウェ", "ホ"),
    "sc" -> Seq("サ", "シ", "ス", "セ", "ソ"))

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
        if(consonants.contains(newConsonant) || isOverlapped){
          sounds(0) = sounds.head.copy(consonant = Some(newConsonant))
          sounds
        } else {
          Sound(Some(c.toString), None) +: sounds
        }
      }
    }.getOrElse(Array.empty[Sound])
  }

  def convert(sounds: Seq[Sound]): String = {
    val kVowels: Seq[Katakana] = sounds.zipWithIndex.collect{
      case (sound, index) if !(sound.consonant.isEmpty && sound.vowel.isEmpty)=>
        sound.vowel.map{v =>
          val nexts: (Option[Sound], Option[Sound]) =
            if (index == sounds.size - 1) (None, None)
            else if (index == sounds.size - 2) (Some(sounds(index + 1)), None)
            else (Some(sounds(index + 1)), Some(sounds(index + 2)))

          EnglishVowel.convert(sound.consonant, v, nexts, sounds.size)
        }.getOrElse(EnglishVowel.getDefaultVowel(sound.consonant.get))
    }
    //get the first katakana in vowel and convert consonant to kantakana
    ""
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
