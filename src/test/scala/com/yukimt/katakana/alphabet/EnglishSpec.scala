package com.yukimt.katakana.alphabet

import org.specs2.mutable.Specification

class EnglishSpec extends Specification{
  "English" should {
    "decompose" in {
      English.decompose("photography") === Array(Sound("ph", "o"), Sound("t", "o"), Sound("g", ""), Sound("r", "a"), Sound("ph", "y"))
      English.decompose("soccer") === Array(Sound("s", "o"), Sound("cc", "er"))
      English.decompose("feature") === Array(Sound("f", "ea"), Sound("t", "ure"))
    }

    "convert" in {
      English.convert(Array(Sound("ph", "o"), Sound("t", "o"), Sound("g", ""), Sound("r", "a"), Sound("ph", "y"))) === "フォトグラフィ"
    }

    "decompose & convert" in {
      English.convert(English.decompose("size")) === "サイズ"
      English.convert(English.decompose("serve")) === "サーブ"
      English.convert(English.decompose("she")) === "シー"
      English.convert(English.decompose("what")) === "ワット"
      English.convert(English.decompose("trial")) === "トライアル"
      English.convert(English.decompose("station")) === "ステーション"
      English.convert(English.decompose("passion")) === "パッション"
      English.convert(English.decompose("fusion")) === "フュージョン"
      English.convert(English.decompose("airplane")) === "エアープレーン"
      English.convert(English.decompose("which")) === "ウィッチ"
      English.convert(English.decompose("feature")) === "フィーチャー"
      English.convert(English.decompose("centre")) === "センター"
      English.convert(English.decompose("fish")) === "フィッシュ"
      English.convert(English.decompose("sharp")) === "シャープ"
      English.convert(English.decompose("organization")) === "オーガニゼーション"
    }
  }
} 
