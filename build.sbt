name := "understanding-computation"

version := "0.0.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",//para poder ver feature warnings al compilar
  "-language:postfixOps", //para cosas como '5 seconds'
  "-language:implicitConversions",
  "-language:existentials",
  "-language:higherKinds",
  "-unchecked",
  "-language:reflectiveCalls", // para poder utilizar el .$each de la librería de mongodb
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",        // N.B. doesn't work well with the ??? hole
  //"-Ywarn-numeric-widen",
  //"-Ywarn-value-discard", // No muy buena idea combinar esto con akka
  "-Xfuture"
)

resolvers ++= Seq(
    "Sonatype Releases"   at "https://oss.sonatype.org/content/repositories/releases",
    "Sonatype Snapshots"  at "https://oss.sonatype.org/content/repositories/snapshots"
)

val scalazV = "7.1.0"

libraryDependencies ++= Seq(
  "org.scalaz"              %%  "scalaz-core"                   % scalazV,
  "org.scala-lang.modules"  %%  "scala-parser-combinators"      % "1.0.4",
  "junit"                   %   "junit"                         % "4.10"    % "test",
  "org.scalatest"           %%  "scalatest"                     % "2.2.1"   % "test",
  "org.scalacheck"          %%  "scalacheck"                    % "1.11.6"  % "test"
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")