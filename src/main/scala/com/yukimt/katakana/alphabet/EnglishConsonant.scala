package com.yukimt.katakana
package alphabet

trait EnglishConsonant{
  def candidates: Seq[String]
  def default: String

  protected def convert(vowel: Katakana)  = {
    if(vowel.isEmpty) default
    else candidates(vowelToInt(vowel.head)) + vowel.tail
  }
  
  protected def vowelToInt(vowel: Char) = vowel match {
    case 'ア' => 0
    case 'イ' => 1
    case 'ウ' => 2
    case 'エ' => 3
    case 'オ' => 4
    case 'ユ' => 5
  }
}
object EnglishConsonant {
  val consonants = Map(
    "b" -> Normal(Seq("バ", "ビ", "ブ", "ベ", "ボ", "ビュ"), "ブ"),
    "c" -> C,
    "d" -> Sokuon(Seq("ダ", "ディ", "ドゥ", "デ", "ド", "デュ"), "ド"),
    "f" -> Normal(Seq("ファ", "フィ", "フ", "フェ", "フォ", "フュ"), "フ"),
    "g" -> G,
    "h" -> Normal(Seq("ハ", "ヒ", "フ", "ヘ", "ホ", "ヒュ"), "フ"),
    "j" -> Normal(Seq("ジャ", "ジ", "ジュ", "ジェ", "ジョ", "ジュ"), "ジュ"),
    "k" -> Sokuon(Seq("カ", "キ", "ク", "ケ", "コ", "キュ"), "ク"),
    "l" -> Normal(Seq("ラ", "リ", "ル", "レ", "ロ", "リュ"), "ル"),
    "m" -> M,
    "n" -> Normal(Seq("ナ", "ニ", "ヌ", "ネ", "ノ", "ニュ"), "ン"),
    "p" -> Sokuon(Seq("パ", "ピ", "プ", "ペ", "ポ", "ピュ"), "プ"),
    "q" -> Normal(Seq("ク", "キ", "ク", "クェ", "クォ", "キュ"), "ク"),
    "r" -> Normal(Seq("ラ", "リ", "ル", "レ", "ロ", "ル"), "アー"),
    "s" -> S,
    "t" -> T,
    "v" -> Normal(Seq("バ", "ビ", "ブ", "ベ", "ボ", "ビュ"), "ブ"),
    "w" -> Normal(Seq("ワ", "ウィ", "ウ", "ウェ", "ウォ", "ウ"), "ウ"),
    "x" -> Sokuon(Seq("クザ", "クジ", "クス", "クゼ", "クゾ", "クジュ"), "クス"),
    "y" -> Normal(Seq("ヤ", "イ", "ユ", "イェ", "ヨ", "ユ"), "イ"),
    "z" -> Normal(Seq("ザ", "ジ", "ズ", "ゼ", "ゾ", "ジュ"), "ズ"),
    "dg" -> G,
    "ch" -> Sokuon(Seq("チャ", "チ", "チュ", "チェ", "チョ", "チュ"), "チ"),
    "tch" -> Sokuon(Seq("チャ", "チ", "チュ", "チェ", "チョ", "チュ"), "チ"),
    "ck" -> Sokuon(Seq("カ", "キ", "ク", "ケ", "コ", "キュ"), "ク"),
    "sh" -> Sokuon(Seq("シャ", "シ", "シュ", "シェ", "ショ", "シュ"), "シュ"),
    "gh" -> Gh,
    "ph" -> Normal(Seq("ファ", "フィ", "フ", "フェ", "フォ", "フュ"), "フ"),
    "ps" -> Normal(Seq("サ", "シ", "ス", "セ", "ソ", "ス"), "ップス"),
    "th" -> Normal(Seq("サ", "シ", "ス", "セ", "ソ", "ス"), "ス"),
    "wh" -> Normal(Seq("ワ", "ウィ", "フ", "ウェ", "ホ", "フ"), "フ"),
    "ff" -> Sokuon(Seq("ファ", "フィ", "フ", "フェ", "フォ", "フュ"), "フ"),
    "ts" -> Sokuon(Seq("タ", "チ", "ツ", "テ", "ト", "ツ"), "ツ"),
    "sc" -> Normal(Seq("サ", "シ", "ス", "セ", "ソ", "ス"), "ス"),
    "cc" -> Normal(Seq("クサ", "クシ", "クス", "クセ", "クソ", "クシュ"), "クス")
  )

  case class Normal(candidates: Seq[String], default: String) extends EnglishConsonant{
    if(candidates.size != 6)
      throw new IllegalArgumentException("the size of candidates needs to be 6")

    def getKatakana(vowel: Katakana) = convert(vowel)
  }

  case class Sokuon(candidates: Seq[String], default: String) extends EnglishConsonant{
    if(candidates.size != 6)
      throw new IllegalArgumentException("the size of candidates needs to be 6")

    def getKatakana(vowel: Katakana, beforeVowel: Option[Katakana], isLast: Boolean, isOverlapped: Boolean) = {
      if(vowel.isEmpty && beforeVowel.exists(_.size == 1) && (isLast || isOverlapped)) "ッ" + default
      else convert(vowel)
    }
  }

  case object G extends EnglishConsonant{
    val candidates = Seq("ガ", "ギ", "グ", "ジェ", "ゴ", "グ")
    val default = "グ"

    def getKatakana(kVowel: Katakana, aVowel: Alphabet, beforeVowel: Option[Katakana], isLast: Boolean, isFirst: Boolean) = {
      if(kVowel.isEmpty && beforeVowel.exists(_.size == 1) && isLast){
        "ッ" + default
      } else if(kVowel.isEmpty && aVowel == "e" && isLast){
        "ジ"
      } else if (kVowel == "エイ") {
        "ゲイ"
      } else if (kVowel == "エー") {
        "ゲー"
      } else if (kVowel == "ア" && aVowel == "a" && isFirst) {
        "ギャ"
      } else convert(kVowel)
    }
  }

  case object M extends EnglishConsonant{
    val candidates = Seq("マ", "ミ", "ム", "メ", "モ", "ミュ")
    val default = "ン"

    def getKatakana(vowel: Katakana, isLast: Boolean) = {
      if(vowel.isEmpty && isLast){
        "ム"
      } else convert(vowel) 
    }
  }
  
  case object S extends EnglishConsonant{
    val candidates = Seq("サ", "シ", "ス", "セ", "ソ", "ス")
    val default = "ス"
    val subCandidates = Seq("ザ", "ジ", "ズ", "ゼ", "ゾ", "ジュ")
    val subDefault = "ズ"

    def subConvert(vowel: Katakana) = 
      if(vowel.isEmpty) subDefault
      else subCandidates(vowelToInt(vowel.head)) + vowel.tail

    def getKatakana(kVowel: Katakana, aVowel: Alphabet, beforeSound: Option[Sound], isOverlapped: Boolean, isLast: Boolean) = {
      if(aVowel == "io" && isOverlapped){
        "ッショ"
      } else if(aVowel == "io"){
        "ジョ"
      } else if (!beforeSound.isEmpty && !beforeSound.exists(s => s.vowel == "ou") && !isOverlapped && isLast && (aVowel == "e" || aVowel == "y" || beforeSound.exists(n => !Set("c", "f", "k", "p", "s", "t").contains(n.consonant) && n.consonant.size < 2 && n.vowel != "u"))) {
        subConvert(kVowel)
      } else convert(kVowel)
    }
  }
  
  case object T extends EnglishConsonant{
    val candidates = Seq("タ", "ティ", "トゥ", "テ", "ト", "チュ")
    val default = "ト"
    
    def getKatakana(kVowel: Katakana, aVowel: Alphabet, beforeVowel: Option[Katakana], isLast: Boolean) = {
      if(aVowel == "io") "ショ"
      else if(aVowel == "ure") "チャー"
      else if(kVowel.isEmpty && beforeVowel.exists(_.size == 1) && isLast) "ッ" + default
      else convert(kVowel)
    }
  }
  
  case object Gh extends EnglishConsonant{
    val candidates = Seq("ガ", "ギ", "グ", "ジェ", "ゴ", "グ")
    val default = "フ"
    
    def getKatakana(kVowel: Katakana, aVowel: Alphabet, beforeVowel: Option[Alphabet], isLast: Boolean) = {
      if(isLast && beforeVowel.contains("ou") && aVowel.isEmpty) default
      else if(aVowel.isEmpty) ""
      else convert(kVowel)
    }
  }

  case object C extends EnglishConsonant{
    val candidates = Seq("カ", "キ", "ク", "ケ", "コ", "キュ")
    val subCandidates = Seq("サ", "シ", "ス", "セ", "ソ", "ス")
    val default = "ク"
    val subDefault = "ス"

    def subConvert(vowel: Katakana) = subCandidates(vowelToInt(vowel.head)) + vowel.tail 
    def getKatakana(kVowel: Katakana, aVowel: Alphabet) = {
      if(kVowel.isEmpty && aVowel.nonEmpty){
        subDefault
      } else if (aVowel.isEmpty){
        default
      } else {
        aVowel.head match {
          case 'a' if kVowel == "ア" => "キャ"
          case 'a' => convert(kVowel)
          case 'i' => subConvert(kVowel)
          case 'u' => convert(kVowel)
          case 'e' => subConvert(kVowel)
          case 'o' => convert(kVowel)
        }
      }
    }
  }
}
