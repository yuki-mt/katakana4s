package com.yukimt.katakana

package object dictionary{
  type Term = String
  type Reading = String

  val OtherLetter = '#' // letter for non-alphabet
  val RELetter = '/' // letter for regular expression
  case class Index(letter: Char, term: Term, reading: Reading)
}
