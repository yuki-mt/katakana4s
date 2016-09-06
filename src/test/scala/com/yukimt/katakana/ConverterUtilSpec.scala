package com.yukimt.katakana

import org.specs2.mutable.Specification

class ConveterUtilSpec extends Specification{
  "ConveterUtil" should {
    "is alphabet" in {
      ConverterUtil.isAlphabet("fewDa") === true
      ConverterUtil.isAlphabet("aw2") === false
      ConverterUtil.isAlphabet("ケ") === false
    }

    "is katanaka" in {
      ConverterUtil.isKatakana("altogether") === false
      ConverterUtil.isKatakana("イェーイ") === true
    }

    "split word" in {
      ConverterUtil.splitWord("オールtogether昨日") === Array("オール", "together", "昨日")
    }
  }
}
