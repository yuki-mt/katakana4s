package com.yukimt.katakana.tokenizer

trait Tokenizer{
  def tokenize(str: String):Seq[Token] 
}
