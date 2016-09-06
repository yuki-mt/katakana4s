package com.yukimt.katakana

import org.specs2.mutable.Specification

class ConveterUtilSpec extends Specification{
  "ConveterUtil" should {
    "is alphabet" in {
      ConverterUtil.isAllAlphabet("fewDa") === true
      ConverterUtil.isAllAlphabet("aw2") === false
      ConverterUtil.isAllAlphabet("ケ") === false
    }

    "is katanaka" in {
      ConverterUtil.isAllKatakana("altogether") === false
      ConverterUtil.isAllKatakana("イェーイ") === true
    }

    "split word" in {
      ConverterUtil.splitWord("オールtogether昨日") === Array("オール", "together", "昨日")
    }
  }
}
