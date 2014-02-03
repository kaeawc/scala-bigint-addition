name := "scala-bigint-addition"

version := "0.1"

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "SonaType" at "https://oss.sonatype.org/content/groups/public/org/scalatest/"
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.3",
  "org.mockito" % "mockito-all" % "1.9.5"
)

scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:postfixOps",
  "-language:implicitConversions"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

