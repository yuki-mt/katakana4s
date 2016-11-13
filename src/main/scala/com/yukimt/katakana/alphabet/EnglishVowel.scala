package com.yukimt.katakana
package alphabet

object EnglishVowel{
  private val shortVowels = Map('a'->"ア", 'i'->"イ", 'u'->"ウ", 'e'->"エ", 'o'->"オ", 'y'->"イ", 'w'->"ウ")
  private val longVowels = Map('a'->"エー", 'i'->"アイ", 'u'->"ユー", 'e'->"イー", 'o'->"オー", 'y'->"アイ", 'w'->"ウ")
  private val multiVowels = Map("ie"->"イー", "uy"->"アイ", "au"->"オー", "aw"->"オー", "eau"->"ユー", "eu"->"ユー", "io"->"イオ", "ou"->"アウ", "ye"->"イエ", "iew"->"ユー", "oo"->"ウー")

  def convert(consonant: Alphabet, _vowel: Alphabet, nexts:(Option[Sound], Option[Sound]), size: Int): Katakana = {
    val vowel = if (_vowel.endsWith("re") && nexts._1.isEmpty) {
      _vowel.dropRight(1)
    } else _vowel

    if((vowel == "e" || vowel == "ue") && nexts._1.isEmpty && size > 1){
      //ignore e if sounds end with e and the size of sounds is more than 1
      if(consonant.isEmpty)
        throw new IllegalArgumentException("needs to have consonant")
      ""
    } else if(vowel.size == 1 || (vowel.size == 2 && vowel.endsWith("r"))){
      if(vowel == "r") "アー"
      else {
        val subVowel = vowel.head
        val subKatakana =
          if (!vowel.endsWith("r") && nexts._2.isEmpty && nexts._1.exists(n => n.consonant.size == 1 && Set("e", "or", "er", "ue").contains(n.vowel))) {
            //longvowels if vowel + consonant(1 letter) + [e, or, er]
            longVowels(subVowel)
          } else if (!vowel.endsWith("r") && nexts._1.exists(n => n.consonant.size == 1 && n.vowel.isEmpty) && nexts._2.contains(Sound("l", "e"))) {
            //longvowels if vowel + consonant(1 letter) + le
            longVowels(subVowel)
          } else if (nexts._2.isEmpty && nexts._1.contains(Sound("ght", ""))) {
            //longvowels if vowel + ght
            longVowels(subVowel)
          } else if (nexts._1.exists(n => (n.consonant == "t" || n.consonant == "s") && n.vowel == "io") && nexts._2.contains(Sound("n", ""))) {
            //longvowels if vowel + (tion or sion)
            longVowels(subVowel)
          } else if(nexts._1.isEmpty && vowel.size == 1 && vowel != "y" && vowel != "a") {
            longVowels(subVowel)
          } else {
            shortVowels(subVowel)
          }

        if (vowel endsWith "r") addRSound(subKatakana)
        else subKatakana
      }
    } else if(vowel == "ie" && nexts._2.isEmpty && nexts._1.contains(Sound("t", ""))){
      "アイエ"
    } else if(vowel == "ia" && nexts._2.isEmpty && nexts._1.exists(_.consonant == "l")){
      "アイア"
    } else if(vowel == "ou" && nexts._1.exists(n => n.consonant.nonEmpty && n.vowel.isEmpty) && nexts._2.contains(Sound("l", "e"))){
      "ア"
    } else if(vowel == "ou" && nexts._2.isEmpty && nexts._1.contains(Sound("gh", ""))){
      "ア"
    } else if(consonant == "q"){
      vowel match {
        case "ui" => "ウイ"
        case "ua" => "ウオ"
        case "ue" => "ウエ"
        case _ => "ウ"
      }
    } else if(multiVowels contains vowel){
      multiVowels(vowel)
    } else if(vowel.endsWith("r") && multiVowels.contains(vowel.dropRight(1))){
      addRSound(multiVowels(vowel.dropRight(1)))
    } else if(vowel endsWith "r"){
      addRSound(longVowels(vowel.head))
    } else {
      longVowels(vowel.head)
    }
  }

  private def addRSound(str: Katakana) = {
    if (str == "オ") {
      "オー"
    } else if (str.size == 1) {
      "アー"
    } else if (str endsWith "ー") {
      str.dropRight(1) + "アー"
    } else {
      str + "アー"
    }
  }
}
