package com.yukimt.katakana.alphabet

import org.specs2.mutable.Specification

class EnglishSpec extends Specification{
  "English" should {
    "decompose" in {
      English.decompose("photography") === Array(Sound(Some("ph"), Some("o")), Sound(Some("t"), Some("o")), Sound(Some("g"), None), Sound(Some("r"), Some("a")), Sound(Some("ph"), Some("y")))
      English.decompose("soccer") === Array(Sound(Some("s"), Some("o")), Sound(Some("cc"), Some("er")))
    }
  }
} 
