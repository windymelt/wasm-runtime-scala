val scala3Version = "3.4.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scodec-exercise",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.18",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test",
    libraryDependencies ++= Seq(
      "org.scodec" %% "scodec-core" % "2.2.2",
      "org.scodec" %% "scodec-bits" % "1.1.38",
    ),
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.3",
  )
