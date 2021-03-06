name := "bidding-agent"
version := "0.1"
scalaVersion := "2.12.6"
organization := "assignment"

lazy val akkaHttpVersion  = "10.1.1"
lazy val akkaVersion      = "2.5.12"
lazy val scalaTestVersion = "3.0.5"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"            % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-xml"        % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream"          % akkaVersion,

  "com.typesafe.akka" %% "akka-http-testkit"    % akkaHttpVersion  % Test,
  "com.typesafe.akka" %% "akka-testkit"         % akkaVersion      % Test,
  "com.typesafe.akka" %% "akka-stream-testkit"  % akkaVersion      % Test,
  "org.scalatest"     %% "scalatest"            % scalaTestVersion % Test
)