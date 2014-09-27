name := "ReliefExtruder"

version := "0.1"

scalaVersion := "2.11.1"

fork := true

scalacOptions ++= Seq(
	"-deprecation",
	"-feature",
	"-Ydelambdafy:method",
	"-target:jvm-1.7",
	"-language:implicitConversions"
)

resolvers ++= Seq(
//  "Typesafe Repository"       at "http://repo.typesafe.com/typesafe/releases/",
  "Scala Tools Repo Releases" at "http://scala-tools.org/repo-releases" //,
//  "spray"                     at "http://repo.spray.io/"
)

libraryDependencies ++= Seq(
//  "commons-codec" % "commons-codec" % "1.5",
//  "io.spray" %% "spray-httpx" % "1.3.1",
//  "io.spray" %% "spray-client" % "1.3.1",
//  "io.spray" %% "spray-json" % "1.2.6",
//  "com.typesafe.akka" %% "akka-actor" % "2.3.3",
//  "com.typesafe.akka" %% "akka-remote" % "2.3.3",
//  "org.jogamp.gluegen" % "gluegen-rt" % "2.1.0" classifier "natives-macosx-universal" classifier "",
//  "org.jogamp.jogl" % "jogl-all" % "2.1.0" classifier "natives-macosx-universal" classifier "",
//  "com.fasterxml.jackson.module" % "jackson-module-scala_2.11" % "2.4.1",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2"
)