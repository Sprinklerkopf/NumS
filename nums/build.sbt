scalaVersion := "2.13.3"

name := "NumS"
version := "1.0"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
unmanagedJars in Compile += file("lib/lwjgl-glfw.jar")
