package com.yukimt

package object katakana{
  trait ConversionMode
  object ConversionMode{
    case object Space extends ConversionMode
    case object EnglishNoSpace extends ConversionMode
    case object NoSpace extends ConversionMode
  }
}
