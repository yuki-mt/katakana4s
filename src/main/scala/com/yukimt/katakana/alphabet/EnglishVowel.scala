package com.yukimt.katakana
package alphabet

object EnglishVowel{
  private val shortVowels = Map('a'->"ア", 'i'->"イ", 'u'->"ア", 'e'->"エ", 'o'->"オ", 'y'->"イ", 'w'->"ウ")
  private val longVowels = Map('a'->"エイ", 'i'->"アイ", 'u'->"ユー", 'e'->"イー", 'o'->"オー", 'y'->"アイ", 'w'->"ウ")
  private val multiVowels = Map("ie"->"イー", "uy"->"アイ", "au"->"オー", "aw"->"オー", "eau"->"ユー", "eu"->"ユー", "io"->"イオ", "ou"->"アウ", "ye"->"イエ", "iew"->"ユー", "oo"->"ウー", "oy"->"オイ", "ew"->"ユー", "ure"-> "ユアー", "ore"->"オアー", "oor"->"オアー", "oi" -> "オイ")

  def convert(consonant: Alphabet, _vowel: Alphabet, nexts:(Option[Sound], Option[Sound]), size: Int, isNextsLast: Boolean, isFirst: Boolean): Katakana = {
    val vowel = 
      if (_vowel == "re") "r"
      else _vowel

    val isLast = nexts._1.isEmpty || nexts._2.isEmpty && nexts._1.contains(Sound("l", "y"))
    val isNextLast = nexts._2.isEmpty || nexts._2.contains(Sound("l", "y")) && isNextsLast
    if((vowel == "e" || vowel == "ue") && isLast && size > 1){
      //ignore e if sounds end with e and the size of sounds is more than 1
      if(consonant.isEmpty)
        throw new IllegalArgumentException("needs to have consonant")
      ""
    } else if(vowel == "ie" && isNextLast && nexts._1.contains(Sound("t", ""))){
      "アイエ"
    } else if(isFirst && size > 2 && vowel == "e" && Set("b", "d", "p", "r").contains(consonant)){
      "イ"
    } else if(vowel == "ia" && isNextLast && nexts._1.exists(_.consonant == "l")){
      "アイア"
    } else if(vowel == "oo" && isNextLast && nexts._1.exists(n => Set("ck", "t", "k", "d") contains n.consonant)){
      "ウ"
    } else if(vowel == "ou" && nexts._1.exists(n => n.consonant.nonEmpty && n.vowel.isEmpty) && nexts._2.contains(Sound("l", "e"))){
      "ア"
    } else if(vowel == "ou" && isNextLast && nexts._1.contains(Sound("gh", ""))){
      "ア"
    } else if(vowel == "ou" && isNextLast && nexts._1.contains(Sound("s", ""))){
      "ア"
    } else if(vowel == "a" && (nexts._1.exists(n => n.consonant == "ll") || nexts._1.contains(Sound("l", "")) && nexts._2.exists(n => n.consonant == "k"))){
      "オー"
    } else if(vowel == "e" && nexts._1.exists(n => n.consonant == "n") && consonant == "v"){
      "ウ"
    } else if(size > 2 && vowel == "e" && nexts._1.contains(Sound("d", "")) && isNextLast){
      if(consonant == "t" || consonant == "d") "エ"
      else ""
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
      addRSound(multiVowels(vowel.dropRight(1)), isLast)
    } else if(vowel.size > 2 && vowel.endsWith("r")){
      addRSound(longVowels(vowel.head), isLast)
    } else if(vowel.size > 1 && !vowel.endsWith("r")){
      longVowels(vowel.head)
    } else {
      if(vowel == "r") "アー"
      else if (vowel == "y" && isLast) "イー"
      else {
        val subVowel = vowel.head
        val subKatakana =
          if (!vowel.endsWith("r") && isNextLast && nexts._1.exists(n => n.consonant.size == 1 && Set("e", "or", "er", "ue").contains(n.vowel))) {
            //longvowels if vowel + consonant(1 letter) + [e, or, er]
            longVowels(subVowel)
          } else if(nexts._1.contains(Sound("l", "")) && nexts._2.exists(n => n.consonant == "d" && (isNextsLast || n.vowel.nonEmpty))){
            //longvowels if vowel + ld[vowel]
            longVowels(subVowel)
          } else if (!vowel.endsWith("r") && nexts._1.exists(n => n.consonant.size == 1 && n.vowel.isEmpty) && nexts._2.contains(Sound("l", "e"))) {
            //longvowels if vowel + consonant(1 letter) + le
            longVowels(subVowel)
          } else if (!vowel.endsWith("r") && nexts._1.exists(n => n.consonant.size == 1 && n.vowel == "e") && nexts._2.contains(Sound("d", ""))) {
            //longvowels if vowel + consonant(1 letter) + ed
            longVowels(subVowel)
          } else if (nexts._1.contains(Sound("gh", ""))) {
            //longvowels if vowel + gh
            longVowels(subVowel)
          } else if (nexts._1.exists(n => (n.consonant == "t" || n.consonant == "s") && n.vowel == "io") && nexts._2.contains(Sound("n", ""))) {
            //longvowels if vowel + (tion or sion)
            longVowels(subVowel)
          } else if(isLast && vowel.size == 1 && vowel != "y" && vowel != "a") {
            longVowels(subVowel)
          } else if(nexts._1.exists(n => n.vowel == "ou") && nexts._2.contains(Sound("s", ""))) {
            longVowels(subVowel)
          } else {
            shortVowels(subVowel)
          }

        if (vowel endsWith "r") addRSound(subKatakana, isLast)
        else subKatakana
      }
    }
  }

  private def addRSound(str: Katakana, isLast: Boolean) = {
    if (str == "オ" && !isLast) {
      "オー"
    } else if (str.size == 1) {
      "アー"
    } else if (str endsWith "ー") {
      str.dropRight(1) + "アー"
    } else if (str == "エイ") {
      "エアー"
    } else {
      str + "アー"
    }
  }
}
