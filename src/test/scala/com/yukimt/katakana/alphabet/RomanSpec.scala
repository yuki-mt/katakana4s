package com.yukimt.katakana.alphabet

import org.specs2.mutable.Specification

class RomanSpec extends Specification{
  "Roman" should {
    "decompose" in {
      val r = Roman()
      r.decompose("otoshimae") === Array(Sound("", "o"), Sound("t", "o"), Sound("sh", "i"), Sound("m", "a"), Sound("", "e"))
      r.decompose("kanzen") === Array(Sound("k", "a"), Sound("n", ""), Sound("z", "e"), Sound("n", ""))
      r.decompose("fab") must throwA(new IllegalArgumentException(s"Roman word needs to finish vowel: b"))
    }

    "get sokuon" in {
      val r = Roman()
      r.getSokuon("ty") === ("", "ty")
      r.getSokuon("tty") === ("ッ", "ty")
      r.getSokuon("tt") === ("ッ", "t")
      r.getSokuon("tttt") === ("ッッッ", "t")
    }

    "convert" in {
      val r = Roman()
      r.convert(Array(Sound("", "o"), Sound("ttt", "o"), Sound("sh", "i"), Sound("m", "a"), Sound("", "e"))) === "オッットシマエ"
      r.convert(Array(Sound("k", "a"), Sound("n", ""), Sound("z", "e"), Sound("n", ""))) === "カンゼン"
    }
  }
} 
