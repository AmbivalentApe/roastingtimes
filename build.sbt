enablePlugins(ScalaJSPlugin)
libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.0" % "test"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"
testFrameworks += new TestFramework("utest.runner.Framework")
name := "Roasting Times"
scalaVersion := "2.11.7"
scalaJSStage in Global := FastOptStage// core = essentials only. No bells or whistles.
libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "0.10.0"

// React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
jsDependencies += "org.webjars.npm" % "react"     % "0.14.2" / "react-with-addons.js" commonJSName "React"    minified "react-with-addons.min.js"
jsDependencies += "org.webjars.npm" % "react-dom" % "0.14.2" / "react-dom.js" commonJSName "ReactDOM" minified "react-dom.min.js" 