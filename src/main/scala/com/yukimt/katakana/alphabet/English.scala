package com.yukimt.katakana.alphabet

case object English extends Alphabet{
  val vowels = Set('a', 'i', 'u', 'e', 'o')
  val shortVowels = Map('a'->"ア", 'i'->"イ", 'u'->"ウ", 'e'->"エ", 'o'->"オ", 'y'->"イ", 'w'->"ウ")
  val longVowels = Map('a'->"エー", 'i'->"アイ", 'u'->"ウー", 'e'->"イー", 'o'->"オー", 'y'->"アイ", 'w'->"ウ")
  val multiVowels = Map("ie"->"イー", "uy"->"アイ", "au"->"オー", "aw"->"オー", "eau"->"ユー", "eu"->"ユー", "io"->"イオ", "ou"->"アウ", "ye"->"イエ")

  val consonants = Map(
    "b"-> Seq("バ", "ビ", "ブ", "べ", "ボ"),
    "c" -> Seq("カ", "キ", "ク", "ケ", "コ"),
    "d" -> Seq("ダ", "ヂ", "ヅ", "デ", "ド"),
    "f" -> Seq("ファ", "フィ", "フ", "フェ", "フォ"),
    "g" -> Seq("ガ", "ギ", "グ", "ゲ", "ゴ"),
    "h" -> Seq("ハ", "ヒ", "フ", "ヘ", "ホ"),
    "j" -> Seq("ジャ", "ジ", "ジュ", "ジェ", "ジョ"),
    "k" -> Seq("カ", "キ", "ク", "ケ", "コ"),
    "l" -> Seq("ラ", "リ", "ル", "レ", "ロ"),
    "m" -> Seq("マ", "ミ", "ム", "メ", "モ"),
    "n" -> Seq("ナ", "ニ", "ヌ", "ネ", "ノ"),
    "p" -> Seq("パ", "ピ", "プ", "ペ", "ポ"),
    "q" -> Seq("クァ", "クィ", "クゥ", "クェ", "クォ"),
    "r" -> Seq("ラ", "リ", "ル", "レ", "ロ"),
    "s" -> Seq("サ", "シ", "ス", "セ", "ソ"),
    "t" -> Seq("タ", "チ", "ツ", "テ", "ト"),
    "v" -> Seq("ヴァ", "ヴィ", "ヴ", "ヴェ", "ヴォ"),
    "w" -> Seq("ワ", "ウィ", "ウ", "ウェ", "ヲ"),
    "x" -> Seq("ァ", "ィ", "ゥ", "ェ", "ォ"),
    "y" -> Seq("ヤ", "イ", "ユ", "イェ", "ヨ"),
    "z" -> Seq("ザ", "ジ", "ズ", "ゼ", "ゾ"),
    "ch" -> Seq("ヂャ", "ヂィ", "ヂュ", "ヂェ", "ヂョ"),
    "ck" -> Seq("ギャ", "ギィ", "ギュ", "ギェ", "ギョ"),
    "sh" -> Seq("ヒャ", "ヒィ", "ヒュ", "ヒェ", "ヒョ"),
    "gh" -> Seq("キャ", "キィ", "キュ", "キェ", "キョ"),
    "ph" -> Seq("ミャ", "ミィ", "ミュ", "ミェ", "ミョ"),
    "ght" -> Seq("ニャ", "ニィ", "ニュ", "ニェ", "ニョ"),
    "th" -> Seq("ピャ", "ピィ", "ピュ", "ピェ", "ピョ"),
    "wh" -> Seq("ピャ", "ピィ", "ピュ", "ピェ", "ピョ"),
    "sc" -> Seq("リャ", "リィ", "リュ", "リェ", "リョ"))

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
        if(sounds.nonEmpty && (consonants.contains(newConsonant) || isOverlapped)){
          sounds(0) = sounds.head.copy(consonant = Some(newConsonant))
          sounds
        } else {
          Sound(Some(c.toString), None) +: sounds
        }
      }
    }.getOrElse(Array.empty[Sound])
  }

  def convert(sounds: Seq[Sound]): String = {
    //convert vowel
    sounds.zipWithIndex.map{
      case (sound, index) =>
        val kVowel = sound.vowel.map{v =>
          if(v == "e" && index == sounds.size - 1 && sounds.size > 1){
            //ignore e if sounds end with e and the size of sounds is more than 1
            getDefaultVowel(sound.consonant)
          } else if (true){
            //if size of vowel > 1 and contained in multivowels keys, use it
            //multirvowels + "r" -> longrvowel(remove "ー" if exists) + アー
            //if size of vowel > 1 and not contained in multivowels keys,
            //この下のは別々のifブロックにする
            //“ie” + t => アイエット
            //ia + l => アイアル
            //oo: ウー bookとかッが入る場合はウ(dはッはいれない)
            //ou + 子音 + “le”-> 「ア」
            //ou + ghで終わり> 「ア」
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
    str.head match {
      case c if vowels contains c => true
      case 'y' =>
        if(str.length > 1 && (str(1) == 'a' || str(1) == 'u' || str(1) == 'o')) false
        else true
      case 'w' =>
        if(str.length > 1 && (vowels.contains(str(1)) || str(1) == 'y' || str(1) == 'h')) false
        else true
      case 'r' =>
        if(str.length > 1 && (vowels.contains(str(1)) || str(1) == 'y' || str(1) == 'r')) false
        else true
      case _ => false
    }
  }
}
