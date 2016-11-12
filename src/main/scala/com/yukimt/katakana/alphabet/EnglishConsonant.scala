package com.yukimt.katakana
package alphabet

trait EnglishConsonant{
  def isSokuon: Boolean
  def candidates: Seq[String]
}
object EnglishConsonant {
  def vowelToInt(vowel: Char) = vowel match {
    case 'ア' => 0
    case 'イ' => 1
    case 'ウ' => 2
    case 'エ' => 3
    case 'オ' => 4
    case 'ユ' => 5
  }
  case class Normal(candidates: Seq[String], isSokuon: Boolean) extends EnglishConsonant{
    if(candidates.size != 6)
      throw new IllegalArgumentException("the size of candidates needs to be 6")

    /**
     * @param vowel: vowel corresponding to this consonant 
     * @param beforeKatakana: 
     **/
    def getKatakana(vowel: Katakana, before: Option[Sound] = None, isLast: Boolean) = {
      if(vowel.isEmpty)
        throw new IllegalArgumentException("vowel cannot be empty: getKatakana")

      //val sokuon = if(isSokuon && next.isEmpty && vowel.isEbefore.exists(_.vowel.exists(_.size == 1)))
      candidates(vowelToInt(vowel.head)) + vowel.tail
    }
  }
}
