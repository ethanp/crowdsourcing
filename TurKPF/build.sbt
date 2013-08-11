name          := "turkpf"

version       := "0.1"

organization  := "edu.utexas"

scalaVersion  := "2.10.2"

scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-target:jvm-1.7", "-feature", "-Ywarn-adapted-args")

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.14" % "test",
    "org.apache.commons" % "commons-math3" % "3.2"
)
