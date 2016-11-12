package com.yukimt.katakana

import org.specs2.mutable.Specification
import ConverterImplicits._

class ConveterImplicitsSpec extends Specification{
  "ConveterImplicits" should {
    "is alphabet" in {
      "fewDa".isAllAlphabet === true
      "aw2".isAllAlphabet === false
      "ケ".isAllAlphabet === false
    }

    "is katanaka" in {
      "altogether".isAllKatakana === false
      "イェーイ".isAllKatakana === true
    }

    "components" in {
      "オールtogether昨日".toComponents === Array("オール", "together", "昨日")
    }
  }
}
