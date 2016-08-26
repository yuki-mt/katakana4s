package com.yukimt.katakana

import org.specs2.mutable.Specification
import com.atilika.kuromoji.ipadic.Token
import com.atilika.kuromoji.ipadic.Tokenizer
import collection.JavaConversions._

class ConverterSpec extends Specification{
  val tokenizer = new Tokenizer()

  "Converter" should {
    "test" in {
      1 === 1
    }
  }
} 
