package com.yukimt.katakana

package object alphabet{
  case class Sound(consonant: Option[Alphabet], vowel: Option[Alphabet])
}
