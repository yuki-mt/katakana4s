package com.yukimt

package object katakana{
  type Alphabet = String
  type Katakana = String

  trait ConversionMode
  object ConversionMode{
    //Examples below are for English (not for Roman)
    //e.g. "昨日I am Legendという映画を観た" -> "キノウ アイ アム レジェンド トイウ エイガ ヲ ミタ"
    case object Space extends ConversionMode
    //e.g. "昨日I am Legendという映画を観た" -> "キノウ アイアムレジェンド トイウ エイガ ヲ ミタ"
    case object AlphabetNoSpace extends ConversionMode
    //e.g. "昨日I am Legendという映画を観た" -> "キノウアイアムレジェンドトイウエイガヲミタ"
    case object NoSpace extends ConversionMode
  }
}
