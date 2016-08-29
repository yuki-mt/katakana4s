package com.yukimt.katakana.alphabet

/**
 * convert alphabets to Katakana
 */
trait Alphabet{
  /**
   * decompose str to each sound (each sound has consonant and vowel)
   * e.g. (for Roman) "otoshimae" -> "(, o), (t, o), (sh, i), (m ,a), (, e)"
   *      (for English) "player" -> "(p,), (l, ay), (, er)"
   */
  def decompose(str: String): Array[Sound]

  /**
   * convert each sound into Katakana letter and combine them
   * e.g. (for Roman) "(, o), (t, o), (sh, i), (m ,a), (, e)" -> "オトシマエ"
   */
  def convert(sounds: Seq[Sound]): String
}
