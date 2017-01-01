name := "naive-bp"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "0.12",
  "org.scalanlp" %% "breeze-natives" % "0.12",
  "org.scalanlp" %% "breeze-viz" % "0.12",
  "org.typelevel" %% "cats" % "0.8.1",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.4"
)


resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"