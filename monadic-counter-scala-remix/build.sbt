name := """monadic-counter-scala-remix"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

// FP libraries.
libraryDependencies += "org.typelevel" %% "cats" % "0.7.2"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.5"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0")

