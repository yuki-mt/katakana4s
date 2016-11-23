# 何か
ひらがな、漢字、ローマ字をカタカナに変換するScala製ライブラリ
(英語 -> カタカナの変換もいつか・・)

# 導入方法

```build.sbt
resolvers += "katakana4s" at "https://yuki-mt.github.io/katakana4s"
libraryDependencies += "com.yukimt" % "katakana4s_2.11" % "1.0"
```

# 使い方

```
import com.yukimt.katakana.{Converter, ConversionMode}

object Boot extends App {
  val con = new Converter
  println(con.convert("昨日sushi wo ippaiを食べた！")) // output "キノウ スシ ヲ イッパイ タベ タ　！"
  println(con.convert("昨日sushi wo ippaiを食べた！", ConversionMode.AlphabetNoSpace)) // output "キノウ スシヲイッパイ タベ タ　！"
  println(con.convert("昨日sushi wo ippaiを食べた！", ConversionMode.NoSpace)) // output "キノウスシヲイッパイタベタ　！"
}
```
