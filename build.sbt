name := "katakana4s"
version := "1.0"
scalaVersion := "2.11.7"
organization := "com.yukimt"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.specs2" %% "specs2-core" % "2.4.17" % "test",
  "org.specs2" %% "specs2-mock" % "2.4.17" % "test",
  "com.atilika.kuromoji" % "kuromoji-ipadic" % "0.9.0"
)

publishTo := Some(Resolver.file("katakana4s",file("."))(Patterns(true, Resolver.mavenStyleBasePattern)))
