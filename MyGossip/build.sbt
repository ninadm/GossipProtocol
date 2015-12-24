name := "MyGossip"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Typesafe repository mwn" at "http://repo.typesafe.com/typesafe/maven-releases/" 

libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.3.2",
    "com.typesafe.akka" %% "akka-remote" % "2.3.2"
)