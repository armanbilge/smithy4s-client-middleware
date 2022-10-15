ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .enablePlugins(Smithy4sCodegenPlugin)
  // .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "MyApi",
    libraryDependencies ++= Seq(
      "com.disneystreaming.smithy4s" %%% "smithy4s-http4s" % smithy4sVersion.value,
      "org.http4s" %%% "http4s-ember-server" % "0.23.13",
      "org.http4s" %%% "http4s-ember-client" % "0.23.13",
     "org.http4s" %% "http4s-blaze-server" % "0.23.12" // works
    ),
    // scalaJSUseMainModuleInitializer := true,
    // scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)) 
  )
