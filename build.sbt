name := "OVAL-Develop-tool"

version := "0.1"

scalaVersion := "2.13.1"

// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
//libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.0.0-M1"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0"

// https://mvnrepository.com/artifact/org.rogach/scallop
libraryDependencies += "org.rogach" %% "scallop" % "3.3.2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"


// For index storage later
val circeVersion = "0.12.3"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)