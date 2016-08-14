package com.yukimt.katakana.alphabet

import org.specs2.mutable.Specification

class RomanSpec extends Specification{
  "Roman" should {
    "decompose" in {
      val r = Roman()
      r.decompose("otoshimae") === Array(Sound(None, Some("o")), Sound(Some("t"), Some("o")), Sound(Some("sh"), Some("i")), Sound(Some("m"), Some("a")), Sound(None, Some("e")))
      r.decompose("kanzen") === Array(Sound(Some("k"), Some("a")), Sound(Some("n"), None), Sound(Some("z"), Some("e")), Sound(Some("n"), None))
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
      r.convert(Array(Sound(None, Some("o")), Sound(Some("ttt"), Some("o")), Sound(Some("sh"), Some("i")), Sound(Some("m"), Some("a")), Sound(None, Some("e")))) === "オッットシマエ"
      r.convert(Array(Sound(Some("k"), Some("a")), Sound(Some("n"), None), Sound(Some("z"), Some("e")), Sound(Some("n"), None))) === "カンゼン"
    }
  }
} 
