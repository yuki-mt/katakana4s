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
      English.convert(Array(Sound("ph", "o"), Sound("t", "o"), Sound("g", ""), Sound("r", "a"), Sound("ph", "y"))) === "フォトグラフィー"
    }

    "decompose & convert" in {
      English.convert(English.decompose("size")) === "サイズ"
      English.convert(English.decompose("serve")) === "サーブ"
      English.convert(English.decompose("she")) === "シー"
      English.convert(English.decompose("what")) === "ワット"
      English.convert(English.decompose("trial")) === "トライアル"
      English.convert(English.decompose("station")) === "ステイション"
      English.convert(English.decompose("passion")) === "パッション"
      English.convert(English.decompose("fusion")) === "フュージョン"
      English.convert(English.decompose("airplane")) === "エアープレイン"
      English.convert(English.decompose("which")) === "ウィッチ"
      English.convert(English.decompose("feature")) === "フィーチャー"
      English.convert(English.decompose("centre")) === "センター"
      English.convert(English.decompose("fish")) === "フィッシュ"
      English.convert(English.decompose("sharp")) === "シャープ"
      English.convert(English.decompose("organization")) === "オーガニゼイション"
      English.convert(English.decompose("cat")) === "キャット"
      English.convert(English.decompose("switch")) === "スウィッチ"
      English.convert(English.decompose("bed")) === "ベッド"
      English.convert(English.decompose("soy")) === "ソイ"
      English.convert(English.decompose("new")) === "ニュー"
      English.convert(English.decompose("gap")) === "ギャップ"
      English.convert(English.decompose("gun")) === "ガン"
      English.convert(English.decompose("class")) === "クラス"
      English.convert(English.decompose("slowly")) === "スローリー"
      English.convert(English.decompose("cure")) === "キュアー"
      English.convert(English.decompose("floor")) === "フロアー"
      English.convert(English.decompose("sore")) === "ソアー"
      English.convert(English.decompose("doctor")) === "ドクター"
      English.convert(English.decompose("class")) === "クラス"
      English.convert(English.decompose("foot")) === "フット"
      English.convert(English.decompose("cook")) === "クック"
      English.convert(English.decompose("small")) === "スモール"
      English.convert(English.decompose("high")) === "ハイ"
      English.convert(English.decompose("night")) === "ナイト"
      English.convert(English.decompose("rough")) === "ラフ"
      English.convert(English.decompose("fold")) === "フォールド"
      English.convert(English.decompose("seven")) === "セブン"
      English.convert(English.decompose("spilits")) === "スピリッツ"
      English.convert(English.decompose("talk")) === "トーク"
      English.convert(English.decompose("famous")) === "フェイマス"
      English.convert(English.decompose("fought")) === "ファウト"
      English.convert(English.decompose("stopped")) === "ストップド"
      English.convert(English.decompose("dated")) === "デイテッド"
      English.convert(English.decompose("freeze")) === "フリーズ"
      English.convert(English.decompose("present")) === "プレゼント"
      English.convert(English.decompose("use")) === "ユーズ"
      English.convert(English.decompose("rose")) === "ローズ"
      English.convert(English.decompose("house")) === "ハウス"
      English.convert(English.decompose("news")) === "ニュース"
      English.convert(English.decompose("nurse")) === "ナース"
      English.convert(English.decompose("visit")) === "ビジット"
      English.convert(English.decompose("was")) === "ワズ"
    }
  }
} 
