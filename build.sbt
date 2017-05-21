enablePlugins(ScalaJSPlugin)

name := "Sketch"

scalaVersion := "2.11.8"

lazy val root = project.in(file(".")).
  aggregate(sketchJS, sketchJVM).
    settings(
      publish := {},
      publishLocal := {}
    )
    
lazy val sketch = crossProject.in(file(".")).
  settings(
    name := "sketch",
    scalaVersion := "2.11.8",
    version := "0.1"
  ).
  jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.102-R11",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
  ).
  jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.0"
  )
  
lazy val sketchJVM = sketch.jvm
lazy val sketchJS = sketch.js
