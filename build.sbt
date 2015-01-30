scalaVersion in ThisBuild := "2.11.4"

libraryDependencies in ThisBuild += "org.spire-math" %% "spire" % "0.9.1-SNAPSHOT"

jmhSettings

lazy val poly = project.
  in(file("poly"))

lazy val turn = project.
  in(file("turn"))

lazy val benchmark = project.
  in(file("benchmark")).
  dependsOn(poly, turn)
