name := "Typelevel Programming"

version := "1.0"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-language:experimental.macros")

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core" % "7.0.4",
  "com.chuusai"    % "shapeless_2.10.3" % "2.0.0-M1",
  "org.spire-math" %% "spire"       % "0.6.1"
)
