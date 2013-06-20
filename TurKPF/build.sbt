// import AssemblyKeys._

name          := "turkpf"

version       := "0.1"

organization  := "edu.utexas"

scalaVersion  := "2.10.2"

scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-target:jvm-1.7", "-feature", "-Ywarn-adapted-args")

libraryDependencies ++= Seq(
//    "org.slf4j" % "slf4j-api" % "1.7.5",
//    "org.slf4j" % "jul-to-slf4j" % "1.7.5",
//    "org.slf4j" % "log4j-over-slf4j" % "1.7.5",
//    "ch.qos.logback" % "logback-core" % "1.0.11",
//    "ch.qos.logback" % "logback-classic" % "1.0.11",
//    "org.json4s" %% "json4s-native" % "3.2.4",
//    "org.json4s" %% "json4s-jackson" % "3.2.4",
//    "com.yammer.dropwizard" % "dropwizard-core" % "0.6.2",
//    "com.massrelevance" %% "dropwizard-scala" % "0.6.2",
    "org.specs2" %% "specs2" % "1.14" % "test",
    "org.apache.commons" % "commons-math3" % "3.2"
)

// net.virtualvoid.sbt.graph.Plugin.graphSettings

// assemblySettings

// mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
//   {
//     case PathList("javax", "servlet", xs @ _*) => MergeStrategy.first
//     case "application.conf" => MergeStrategy.concat
//     case "unwanted.txt"     => MergeStrategy.discard
//     case "about.html"       => MergeStrategy.discard
//     case x => old(x)
//   }
// }
