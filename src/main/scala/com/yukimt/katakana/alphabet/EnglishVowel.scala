package com.yukimt.katakana
package alphabet

object EnglishVowel{
  private val shortVowels = Map('a'->"ア", 'i'->"イ", 'u'->"ア", 'e'->"エ", 'o'->"オ", 'y'->"イ", 'w'->"ウ")
  private val longVowels = Map('a'->"エー", 'i'->"アイ", 'u'->"ユー", 'e'->"イー", 'o'->"オー", 'y'->"アイ", 'w'->"ウ")
  private val multiVowels = Map("ii"->"イー", "ie"->"イー", "uy"->"アイ", "au"->"オー", "aw"->"オー", "eau"->"ユー", "eu"->"ユー", "io"->"イオ", "ou"->"アウ", "ye"->"イエ", "iew"->"ユー", "oo"->"ウー", "oy"->"オイ", "ew"->"ユー", "ure"-> "ユアー", "ore"->"オアー", "oor"->"オアー", "oi" -> "オイ", "ow" -> "アウ")

  def convert(consonant: Alphabet, _vowel: Alphabet, before: Option[Sound], nexts:(Option[Sound], Option[Sound], Option[Sound]), size: Int): Katakana = {
    val vowel = 
      if (_vowel == "re") "r"
      else if (_vowel endsWith "yi") _vowel.replace("yi", "i") // playing, dying
      else _vowel

      val isLast = nexts._1.isEmpty || nexts._2.isEmpty && size > 2 && (nexts._1.contains(Sound("l", "y")) || nexts._1.contains(Sound("s", "")) || (nexts._1.contains(Sound("", "e")) && nexts._2.contains(Sound("d", ""))) || (vowel == "i" && nexts._1.contains(Sound("n", "")) && nexts._2.contains(Sound("g", ""))))
      val isNextLast = nexts._2.isEmpty || (nexts._2.contains(Sound("l", "y")) || nexts._2.contains(Sound("s", "")) && nexts._3.isEmpty && size > 2) || nexts._2.contains(Sound("", "e")) && nexts._3.contains(Sound("d", "")) || nexts._1.exists(_.vowel == "i") && nexts._2.contains(Sound("n", "")) && nexts._3.contains(Sound("g", ""))
    if((vowel == "e") && isLast && size > 1){
      //ignore e if sounds end with e and the size of sounds is more than 1
      if(consonant.isEmpty)
        throw new IllegalArgumentException("needs to have consonant")
      ""
    } else if(vowel == "ie" && isNextLast && nexts._1.contains(Sound("t", ""))){
      "アイエ"
    } else if(vowel == "ie" && size == 1){
      "アイ"
    } else if(before.isEmpty && size > 2 && vowel == "e" && Set("b", "d", "r").contains(consonant) && (nexts._1.exists(_.vowel.nonEmpty) ||nexts._2.exists(_.vowel.nonEmpty))){
      "イ"
    } else if(vowel == "ia" && isNextLast && nexts._1.exists(_.consonant == "l")){
      "アイア"
    } else if(vowel == "oo" && isNextLast && nexts._1.exists(n => Set("ck", "t", "k", "d") contains n.consonant)){
      "ウ"
    } else if(vowel == "ou" && nexts._1.exists(n => n.consonant.nonEmpty && n.vowel.isEmpty) && nexts._2.contains(Sound("l", "e"))){
      "ア"
    } else if((vowel == "ou" || vowel == "au") && isNextLast && nexts._1.contains(Sound("gh", ""))){
      "ア"
    } else if(vowel == "e" && nexts._1.contains(Sound("r", "i")) && nexts._2.contains(Sound("n", "")) && nexts._3.contains(Sound("g", ""))){
      "ア"
    } else if(vowel == "o" && consonant.nonEmpty && (nexts._1.contains(Sound("v", "er")) || nexts._1.contains(Sound("v", "e")) && (nexts._2.contains(Sound("r", "i")) || nexts._2.contains(Sound("r", "e")) && nexts._3.contains("d", "")))){
      "ア"
    } else if(vowel == "o" && isLast && (consonant == "t" || consonant == "d")){
      "ウー"
    } else if(vowel == "ow" && isLast && (size > 1 || consonant.size > 1)){
      "オー"
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
      addRSound(multiVowels(vowel.dropRight(1)), isLast, consonant)
    } else if(vowel.size > 2 && vowel.endsWith("r")){
      addRSound((if(vowel.head == 'a') "エイ" else longVowels(vowel.head)), isLast, consonant)
    } else if(vowel.size > 1 && !vowel.endsWith("r")){
      if(vowel.head == 'a') "エイ"
      else longVowels(vowel.head)
    } else {
      if(vowel == "r") "アー"
      else if (vowel == "y" && isLast){
        if(size > 2 || before.exists(_.vowel.nonEmpty)) "イー"
        else "アイ"
      } else if (vowel == "o" && before.isEmpty && consonant.isEmpty && nexts._1.exists(_.vowel.nonEmpty)){
        "オー"
      } else if (vowel == "o" && isNextLast && nexts._1.exists(n => n.consonant == "m" && (n.vowel == "e" || n.vowel == "i"))){
        "ア"
      } else if (vowel == "e" && consonant.contains("p") && nexts._1.exists(_.consonant == "n")){
        ""
      } else {
        val subVowel = vowel.head
        val subKatakana =
          if (!(vowel == "i" && (consonant == "g" || consonant == "z")) && !vowel.endsWith("r") && isNextLast && nexts._1.exists(n => n.consonant.size == 1 && Set("e", "or", "er", "ue").contains(n.vowel))) {
            //longvowels if vowel + consonant(1 letter) + [e, or, er]
            longVowels(subVowel)
          } else if(nexts._1.contains(Sound("l", "")) && nexts._2.exists(n => n.consonant == "d" && (nexts._3.isEmpty || n.vowel.nonEmpty))){
            //longvowels if vowel + ld[vowel]
            longVowels(subVowel)
          } else if (!vowel.endsWith("r") && ((nexts._1.exists(n => n.consonant.size == 1 && n.vowel.isEmpty) && nexts._2.contains(Sound("l", "e"))) || (nexts._1.exists(n => n.consonant.size == 1 && n.vowel == "i") && nexts._2.contains(Sound("n", "")) && nexts._3.contains(Sound("g", ""))))) {
            //longvowels if vowel + consonant(1 letter) + [le, ing]
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
          } else if(nexts._1.isEmpty && vowel.size == 1 && vowel != "y" && vowel != "a") {
            longVowels(subVowel)
          } else if(nexts._1.exists(n => n.vowel == "ou") && nexts._2.contains(Sound("s", ""))) {
            longVowels(subVowel)
          } else if(nexts._1.exists(n => n.vowel == "ure") && isNextLast) {
            longVowels(subVowel)
          } else if(nexts._1.exists(n => n.consonant.size == 1 && n.vowel == "ar")) {
            if(vowel == "u") "ユ"
            else longVowels(subVowel)
          } else {
            shortVowels(subVowel)
          }

        if (vowel endsWith "r") addRSound(subKatakana, isLast, consonant)
        else subKatakana
      }
    }
  }

  private def addRSound(str: Katakana, isLast: Boolean, consonant: Alphabet) = {
    if (str == "オ" && !isLast && consonant != "w") {
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
