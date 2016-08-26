package com.yukimt.katakana.tokenizer

import org.specs2.mutable.Specification

class KuromojiSpec extends Specification{
  "kuromoji tokenizer" should {
    "tokenize" in {
      Kuromoji.tokenize("昨日コーヒーを　　Star Bucksで  飲んだ!！") === Seq(Token("昨日","キノウ"), Token("コーヒー","コーヒー"), Token("を","ヲ"), Token("Star","Star"), Token("Bucks","Bucks"), Token("で","デ"), Token("飲ん","ノン"), Token("だ","ダ"), Token("!！","!！"))
    }
  }
} 
