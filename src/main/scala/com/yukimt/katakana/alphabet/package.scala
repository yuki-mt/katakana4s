package com.yukimt.katakana

package object alphabet{
  case class Sound(consonant: Option[String], vowel: Option[String])

  object EnglishUtil{
    trait Consonant{
      def isSokuon: Boolean
      def word: String
      def candidates: Seq[String]
      def getKatakana: Katakana
    }
  }
}
