package com.yukimt.katakana
package alphabet

object EnglishVowel{
  private val shortVowels = Map('a'->"ア", 'i'->"イ", 'u'->"ウ", 'e'->"エ", 'o'->"オ", 'y'->"イ", 'w'->"ウ")
  private val longVowels = Map('a'->"エー", 'i'->"アイ", 'u'->"ウー", 'e'->"イー", 'o'->"オー", 'y'->"アイ", 'w'->"ウ")
  private val multiVowels = Map("ie"->"イー", "uy"->"アイ", "au"->"オー", "aw"->"オー", "eau"->"ユー", "eu"->"ユー", "io"->"イオ", "ou"->"アウ", "ye"->"イエ", "iew"->"ユー", "oo"->"ウー")

  def convert(consonant: Option[Alphabet], vowel: Alphabet, nexts:(Option[Sound], Option[Sound]), size: Int): Katakana = {
    if((vowel == "e" || vowel == "ue") && nexts._1.isEmpty && size > 1){
      //ignore e if sounds end with e and the size of sounds is more than 1
      consonant match {
        case Some(c) => ""
        case None => throw new IllegalArgumentException("needs to have consonant")
      }
    } else if(vowel.size == 1 || (vowel.size == 2 && vowel.endsWith("r"))){
      if(vowel == "r") "アー"
      else {
        val subVowel = vowel.head
        val subKatakana =
          if (nexts._2.isEmpty && nexts._1.exists(n => n.consonant.exists(c => c.size > 1 && c.distinct.size == 1) && n.vowel.exists(v => Set("e", "or", "er", "el", "le", "ue") contains v))) {
            //longvowels if vowel + consonant(no overlapped) + [e, or, er, el, le]
            longVowels(subVowel)
          } else if (nexts._2.isEmpty && nexts._1.exists(n => n.consonant.contains("ght") && n.vowel.isEmpty)) {
            //longvowels if vowel + ght
            longVowels(subVowel)
          } else if (nexts._1.exists(n => n.consonant.exists(c => c == "t" || c == "s") && n.vowel.contains("io")) && nexts._2.contains(Sound(Some("n"), None))) {
            //longvowels if vowel + (tion or sion)
            longVowels(subVowel)
          } else if (nexts._1.isEmpty) {
            //longvowels if end with a single vowel (e.g. sky) 
            longVowels(subVowel)
          } else {
            shortVowels(subVowel)
          }

        if (vowel endsWith "r") addRSound(subKatakana)
        else subKatakana
      }
    } else if(vowel == "ie" && nexts._2.isEmpty && nexts._1.exists(_.consonant == "t")){
      "アイエ"
    } else if(vowel == "ia" && nexts._2.isEmpty && nexts._1.exists(_.consonant == "l")){
      "アイア"
    } else if(vowel == "ou" && nexts._1.exists(n => n.consonant.isDefined && n.vowel.isEmpty) && nexts._2.contains(Sound(Some("l"), Some("e")))){
      "ア"
    } else if(vowel == "ou" && nexts._2.isEmpty && nexts._1.contains(Sound(Some("gh"), None))){
      "ア"
    } else if(consonant contains "q"){
      vowel match {
        case "ui" => "ウイ"
        case "ua" => "ウオ"
        case "ue" => "ウエ"
        case _ => "ウ"
      }
    } else if(multiVowels contains vowel){
      multiVowels(vowel)
    } else if(vowel.endsWith("r") && multiVowels.contains(vowel.dropRight(1))){
      addRSound(multiVowels(vowel))
    } else if(vowel endsWith "r"){
      addRSound(longVowels(vowel.head))
    } else {
      longVowels(vowel.head)
    }
  }

  private def addRSound(str: Katakana) = {
    if (str.size == 1) {
      "アー"
    } else if (str endsWith "ー") {
      str.dropRight(1) + "アー"
    } else {
      str + "アー"
    }
  }
}
