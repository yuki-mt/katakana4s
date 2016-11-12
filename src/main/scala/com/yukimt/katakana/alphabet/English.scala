package com.yukimt.katakana.alphabet

case object English extends AlphabetConverter{
  val vowels = Set('a', 'i', 'u', 'e', 'o')
  val shortVowels = Map('a'->"ア", 'i'->"イ", 'u'->"ウ", 'e'->"エ", 'o'->"オ", 'y'->"イ", 'w'->"ウ")
  val longVowels = Map('a'->"エー", 'i'->"アイ", 'u'->"ウー", 'e'->"イー", 'o'->"オー", 'y'->"アイ", 'w'->"ウ")
  val multiVowels = Map("ie"->"イー", "uy"->"アイ", "au"->"オー", "aw"->"オー", "eau"->"ユー", "eu"->"ユー", "io"->"イオ", "ou"->"アウ", "ye"->"イエ", "iew"->"ユー")

  val consonants = Map(
    "b"-> Seq("バ", "ビ", "ブ", "べ", "ボ"),
    "c" -> Seq("カ", "キ", "ク", "ケ", "コ"),
    "d" -> Seq("ダ", "ディ", "デュ", "デ", "ド"),
    "f" -> Seq("ファ", "フィ", "フ", "フェ", "フォ"),
    "g" -> Seq("ガ", "ギ", "グ", "ゲ", "ゴ"),
    "h" -> Seq("ハ", "ヒ", "フ", "ヘ", "ホ"),
    "j" -> Seq("ジャ", "ジ", "ジュ", "ジェ", "ジョ"),
    "k" -> Seq("カ", "キ", "ク", "ケ", "コ"),
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
    sounds.zipWithIndex.map{
      case (sound, index) =>
        //convert vowel
        val kVowel = sound.vowel.map{v =>
          if(v == "e" && index == sounds.size - 1 && sounds.size > 1){
            //ignore e if sounds end with e and the size of sounds is more than 1
            getDefaultVowel(sound.consonant)
          } else if (v.size > 1){
            if(v == "ie" && index != sounds.size - 1 && sounds(index + 1).consonant.contains("t"))
              "アイエ"
            else if(v == "ia" && index != sounds.size - 1 && sounds(index + 1).consonant.contains("l"))
              "アイア"
            //oo: ウー bookとかッが入る場合はウ(dはッはいれない)
            //ou + 子音 + “le”-> 「ア」
            //ou + ghで終わり> 「ア」
            //if size of vowel > 1 and contained in multivowels keys, use it
            //multirvowels + "r" -> longrvowel(remove "ー" if exists) + アー
            //if size of vowel > 1 and not contained in multivowels keys, use longvowels of the first one
          } else if (true){
            //longvowels if vowel + consonant(no overlapped) + [e, or, er, el, le]
            //longvowels if vowel + ght
            //longrvowels + "r" -> longrvowel(remove "ー" if exists) + アー
            //基本複合母音のときは1文字目を英語読みにする。(yはアイ)
          } else {
            // use shortvowels
            // if end with "r" -> アー
          } 
        }.getOrElse(getDefaultVowel(sound.consonant))
    }
    //get the first katakana in vowel and convert consonant to kantakana
    ""
  }

  def getDefaultVowel(consonant: Option[String]) = {
    if(consonant.exists(c => c.endsWith("t") || c.endsWith("d"))) "オ"
    else if(consonant.contains("ch")) "イ"
    else "ウ"
  }

  def vowelToInt(vowel: Char) = vowel match {
    case 'ア' => 0
    case 'イ' => 1 
    case 'ウ' => 2
    case 'エ' => 3
    case 'オ' => 4
    case 'ユ' => 5
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
