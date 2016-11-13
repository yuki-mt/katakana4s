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
    "b" -> Normal(Seq("バ", "ビ", "ブ", "べ", "ボ", "ビュ"), "ブ"),
    "c" -> C,
    "d" -> Sokuon(Seq("ダ", "ディ", "デュ", "デ", "ド", "デュ"), "ド"),
    "f" -> Normal(Seq("ファ", "フィ", "フ", "フェ", "フォ", "フュ"), "フ"),
    "g" -> G,
    "h" -> Normal(Seq("ハ", "ヒ", "フ", "ヘ", "ホ", "ヒュ"), "フ"),
    "j" -> Normal(Seq("ジャ", "ジ", "ジュ", "ジェ", "ジョ", "ジュ"), "ジュ"),
    "k" -> Sokuon(Seq("カ", "キ", "ク", "ケ", "コ", "キュ"), "ク"),
    "l" -> Normal(Seq("ラ", "リ", "ル", "レ", "ロ", "ル"), "リュ"),
    "m" -> Normal(Seq("マ", "ミ", "ム", "メ", "モ", "ミュ"), "ン"),
    "n" -> Normal(Seq("ナ", "ニ", "ヌ", "ネ", "ノ", "ニュ"), "ン"),
    "p" -> Sokuon(Seq("パ", "ピ", "プ", "ペ", "ポ", "ピュ"), "プ"),
    "q" -> Normal(Seq("ク", "キ", "ク", "クェ", "クォ", "キュ"), "ク"),
    "r" -> Normal(Seq("ラ", "リ", "ル", "レ", "ロ", "リュ"), "ル"),
    "s" -> S,
    "t" -> Sokuon(Seq("タ", "ティ", "トゥ", "テ", "ト", "チュ"), "ト"),
    "v" -> Normal(Seq("バ", "ヴィ", "ヴ", "ベ", "ボ", "ビュ"), "ブ"),
    "w" -> Normal(Seq("ワ", "ウィ", "ウ", "ウェ", "ウォ", "ウ"), "ウ"),
    "x" -> Sokuon(Seq("クザ", "クジ", "クス", "クゼ", "クゾ", "クジュ"), "クス"),
    "y" -> Normal(Seq("ヤ", "イ", "ユ", "イェ", "ヨ", "ユ"), "イ"),
    "z" -> Normal(Seq("ザ", "ジ", "ズ", "ゼ", "ゾ", "ジュ"), "ズ"),
    "dg" -> G,
    "ch" -> Normal(Seq("チャ", "チ", "チュ", "チェ", "チョ", "チュ"), "チ"),
    "ck" -> Sokuon(Seq("カ", "キ", "ク", "ケ", "コ", "キュ"), "ク"),
    "sh" -> Normal(Seq("シャ", "シ", "シュ", "シェ", "ショ", "シュ"), "シュ"),
    "gh" -> Normal(Seq("ガ", "ギ", "グ", "ジェ", "ゴ", "グ"), "フ"),
    "ph" -> Normal(Seq("ファ", "フィ", "フ", "フェ", "フォ", "フュ"), "フ"),
    "ps" -> Normal(Seq("サ", "シ", "ス", "セ", "ソ", "ス"), "プス"),
    "ght" -> Normal(Seq("タ", "ティ", "トゥ", "テ", "ト", "トゥ"), "ト"),
    "th" -> Normal(Seq("サ", "シ", "ス", "セ", "ソ", "ス"), "ス"),
    "wh" -> Normal(Seq("ワ", "ウィ", "フ", "ウェ", "ホ", "フ"), "フ"),
    "ff" -> Sokuon(Seq("ファ", "フィ", "フ", "フェ", "フォ", "フュ"), ">フ"),
    "sc" -> Normal(Seq("サ", "シ", "ス", "セ", "ソ", "ス"), "ス"),
    "ss" -> Normal(Seq("サ", "シ", "ス", "セ", "ソ", "ス"), "ス")
  )

  case class Normal(candidates: Seq[String], default: String) extends EnglishConsonant{
    if(candidates.size != 6)
      throw new IllegalArgumentException("the size of candidates needs to be 6")

    def getKatakana(vowel: Katakana) = convert(vowel)
  }

  case class Sokuon(candidates: Seq[String], default: String) extends EnglishConsonant{
    if(candidates.size != 6)
      throw new IllegalArgumentException("the size of candidates needs to be 6")

    def getKatakana(vowel: Katakana, beforeVowel: Katakana, isLast: Boolean) = {
      if(vowel.isEmpty && beforeVowel.size == 1 && isLast) "ッ" + default
      else convert(vowel)
    }
  }

  case object G extends EnglishConsonant{
    val candidates = Seq("ガ", "ギ", "グ", "ジェ", "ゴ", "グ")
    val default = "グ"

    def getKatakana(vowel: Katakana, hasAlphabetVowel: Boolean, isLast: Boolean) = {
      if(vowel.isEmpty && hasAlphabetVowel && isLast){
        "ジ"
      } else if (vowel == "エイ") {
        "ゲイ"
      } else convert(vowel)
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

    def getKatakana(vowel: Katakana, isLast: Boolean, beforeConsonant: Alphabet) = {
      if(vowel.isEmpty && isLast && !(Set("f", "p", "k", "th") contains beforeConsonant)){
        "ズ"
      } else convert(vowel)
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
      } else {
        aVowel.head match {
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
