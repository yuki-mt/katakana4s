package com.yukimt.katakana.alphabet

import org.specs2.mutable.Specification

class RomanSpec extends Specification{
  "Roman" should {
    "decompose" in {
      Roman.decompose("otoshimae") === Array(Sound(None, Some("o")), Sound(Some("t"), Some("o")), Sound(Some("sh"), Some("i")),
        Sound(Some("m"), Some("a")), Sound(None, Some("e")))
    }
  }
} 
