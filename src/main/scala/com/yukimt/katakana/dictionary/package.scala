package com.yukimt.katakana

package object dictionary{
  type Term = String
  type Reading = String

  val KanjiLetter = '#' // letter for Kanji
  val RELetter = '/' // letter for regular expression
  case class Index(letter: Char, term: Term, reading: Reading)
}
