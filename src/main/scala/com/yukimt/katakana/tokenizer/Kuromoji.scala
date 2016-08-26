package com.yukimt.katakana.tokenizer

import com.atilika.kuromoji.ipadic.{Tokenizer => KuromojiTokenizer}
import collection.JavaConversions._

object Kuromoji extends Tokenizer{
  val tokenizer = new KuromojiTokenizer()
  def tokenize(str: String) = {
    val filteredStr = str.replaceAll("　", " ").replaceAll(" +", " ")
    val tokens = tokenizer.tokenize(filteredStr).toSeq
    tokens.collect{
      case t if t.getSurface != " " =>
        val reading = if(t.getReading == "*") t.getSurface else t.getReading
        Token(t.getSurface, reading)
    }
  }
}
