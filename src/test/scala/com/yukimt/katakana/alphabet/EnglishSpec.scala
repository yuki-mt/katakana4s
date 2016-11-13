package com.yukimt.katakana.alphabet

import org.specs2.mutable.Specification

class EnglishSpec extends Specification{
  "English" should {
    "decompose" in {
      English.decompose("photography") === Array(Sound("ph", "o"), Sound("t", "o"), Sound("g", ""), Sound("r", "a"), Sound("ph", "y"))
      English.decompose("soccer") === Array(Sound("s", "o"), Sound("cc", "er"))
    }
  }
} 
