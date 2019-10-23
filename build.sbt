organization := "com.github.kmizu"

name := "a2r"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.10"

publishMavenStyle := true

val scaladocBranch = settingKey[String]("branch name for scaladoc -doc-source-url")

scaladocBranch := "master"

scalacOptions in (Compile, doc) ++= { Seq(
  "-sourcepath", baseDirectory.value.getAbsolutePath,
  "-doc-source-url", s"https://github.com/kmizu/s2r/tree/${scaladocBranch.value}€{FILE_PATH}.scala"
)}

crossScalaVersions := Seq("2.11.11", scalaVersion.value)

testOptions in Test += Tests.Argument("-oI")

scalacOptions ++= {
  Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")
}

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "com.github.scaruby" %% "scaruby" % "0.5",
  "com.github.kmizu" %% "scomb" % "0.6",
  "org.scalatest" %% "scalatest" %  "3.0.0"
)

assemblyJarName in assembly := "a2r.jar"

mainClass in assembly := Some("com.github.kmizu.a2r.Main")

initialCommands in console += {
  Iterator(
    "com.github.kmizu.a2r._"
  ).map("import "+).mkString("\n")
}

pomExtra := (
  <url>https://github.com/kmizu/a2r</url>
  <licenses>
    <license>
      <name>The MIT License</name>
      <url>http://www.opensource.org/licenses/MIT</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:kmizu/a2r.git</url>
    <connection>scm:git:git@github.com:kmizu/a2r.git</connection>
  </scm>
  <developers>
    <developer>
      <id>kmizu</id>
      <name>Kota Mizushima</name>
      <url>https://github.com/kmizu</url>
    </developer>
  </developers>
) 

publishTo := {
  val v = version.value
  val nexus = "https://oss.sonatype.org/"
  if (v.endsWith("-SNAPSHOT"))
    Some("snapshots" at nexus+"content/repositories/snapshots")
  else
    Some("releases" at nexus+"service/local/staging/deploy/maven2")
}

credentials ++= {
  val sonatype = ("Sonatype Nexus Repository Manager", "oss.sonatype.org")
  def loadMavenCredentials(file: java.io.File) : Seq[Credentials] = {
    xml.XML.loadFile(file) \ "servers" \ "server" map (s => {
      val host = (s \ "id").text
      val realm = if (host == sonatype._2) sonatype._1 else "Unknown"
      Credentials(realm, host, (s \ "username").text, (s \ "password").text)
    })
  }
  val ivyCredentials   = Path.userHome / ".ivy2" / ".credentials"
  val mavenCredentials = Path.userHome / ".m2"   / "settings.xml"
  (ivyCredentials.asFile, mavenCredentials.asFile) match {
    case (ivy, _) if ivy.canRead => Credentials(ivy) :: Nil
    case (_, mvn) if mvn.canRead => loadMavenCredentials(mvn)
    case _ => Nil
  }
}
