enablePlugins(ScalaJSPlugin)

name := "Promotion"

scalaVersion := "2.11.8"

lazy val root = project.in(file(".")).
  aggregate(promoJS, promoJVM).
    settings(
      publish := {},
      publishLocal := {}
    )
    
lazy val promo = crossProject.in(file(".")).
  settings(
    name := "promo",
    version := "0.1"
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.0"
    // Add JS-specific settings here
  )
  
lazy val promoJVM = promo.jvm
lazy val promoJS = promo.js
