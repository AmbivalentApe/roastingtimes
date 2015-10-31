enablePlugins(ScalaJSPlugin)
libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.0" % "test"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"
testFrameworks += new TestFramework("utest.runner.Framework")
name := "Roasting Times"
scalaVersion := "2.11.7"
scalaJSStage in Global := FastOptStage