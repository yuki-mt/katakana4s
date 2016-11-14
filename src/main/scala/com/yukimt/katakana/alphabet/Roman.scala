package com.yukimt.katakana
package alphabet

case class Roman(userConsonants: Option[Map[String, Seq[String]]] = None) extends AlphabetConverter{
  val vowels = Set('a', 'i', 'u', 'e', 'o')

  val baseConsonants = Map(
    "" -> Seq("ア", "イ", "ウ", "エ", "オ"),
    "b"-> Seq("バ", "ビ", "ブ", "ベ", "ボ"),
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
    "dy" -> Seq("ヂャ", "ヂィ", "ヂュ", "ヂェ", "ヂョ"),
    "gy" -> Seq("ギャ", "ギィ", "ギュ", "ギェ", "ギョ"),
    "hy" -> Seq("ヒャ", "ヒィ", "ヒュ", "ヒェ", "ヒョ"),
    "ky" -> Seq("キャ", "キィ", "キュ", "キェ", "キョ"),
    "my" -> Seq("ミャ", "ミィ", "ミュ", "ミェ", "ミョ"),
    "ny" -> Seq("ニャ", "ニィ", "ニュ", "ニェ", "ニョ"),
    "py" -> Seq("ピャ", "ピィ", "ピュ", "ピェ", "ピョ"),
    "ry" -> Seq("リャ", "リィ", "リュ", "リェ", "リョ"),
    "sy" -> Seq("シャ", "シィ", "シュ", "シェ", "ショ"),
    "ty" -> Seq("チャ", "チィ", "チュ", "チェ", "チョ"),
    "zy" -> Seq("ジャ", "ジィ", "ジュ", "ジェ", "ジョ"),
    "sh" -> Seq("シャ", "シ", "シュ", "シェ", "ショ"),
    "ch" -> Seq("チャ", "チ", "チュ", "チェ", "チョ"),
    "ts" -> Seq("ツァ", "ツィ", "ツ", "ツェ", "ツォ"))

  lazy val consonants = userConsonants.map(c => baseConsonants ++ c).getOrElse(baseConsonants)
  lazy val mnSet = consonants.keys.filter(k => k.startsWith("m") || k.startsWith("n")).toSet

  def decompose(str: String): Array[Sound] = {
    str.headOption.map{ c =>
      if(vowels contains c)
        Sound("", c.toString) +: decompose(str.tail)
      else{
        val sounds = decompose(str.tail)
        if(sounds.isEmpty){
          if(c == 'm' || c == 'n')
            Array(Sound(c.toString, ""))
          else
            throw new IllegalArgumentException(s"Roman word needs to finish vowel: $str")
        } else {
          val consonant = c + sounds.head.consonant
          if((mnSet contains consonant) || (c != 'm' && c != 'n')){
            sounds(0) = sounds.head.copy(consonant = consonant)
            sounds
          } else {
            Sound(c.toString, "") +: sounds
          }
        }
      }
    }.getOrElse(Array.empty[Sound])
  }

  def convert(sounds: Seq[Sound]): String = {
    sounds.map{s =>
      val (sokuon, con) = getSokuon(s.consonant)
      val consonant = if(s.vowel.nonEmpty) { 
        consonants.get(con).map(seq => seq(vowelToInt(s.vowel.head))) match {
          case Some(k) => k
          case None => throw new IllegalArgumentException(s"the consonant is not in expected: ${s.consonant}")
        }
      } else {
        if(con == "m" || con == "n") "ン"
        else throw new IllegalArgumentException(s"Sound needs both consonant and vowel: $s")
      }
      sokuon + consonant
    }.mkString
  }

  def getSokuon(con: Alphabet): (String, String) = {
    if(con.length > 1 && con.head == con(1)){
      val s = getSokuon(con.tail)
      ("ッ" + s._1, s._2)
    }
    else ("", con)
  }

  def vowelToInt(vowel: Char) = vowel match {
    case 'a' => 0
    case 'i' => 1 
    case 'u' => 2
    case 'e' => 3
    case 'o' => 4
  }
}
