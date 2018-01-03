enablePlugins(ScalaJSPlugin)

name := "Edgeworth"
scalaVersion := "2.12.4" // or any other Scala version >= 2.10.2

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
