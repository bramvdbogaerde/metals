package tests

import java.net.URI

import scala.meta.internal.metals.Directories
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.PositionSyntax._
import scala.meta.io.AbsolutePath

import org.eclipse.lsp4j.Location

class FindTextInDependencyJarsSuite
    extends BaseLspSuite("find-text-in-dependency-jars") {

  val akkaVersion = "2.6.16"

  test("find exact string match in .conf file inside jar") {
    val isJavaAtLeast9 = scala.util.Properties.isJavaAtLeast(9.toString)
    val isJavaAtLeast17 = scala.util.Properties.isJavaAtLeast(17.toString)

    for {
      _ <- initialize(
        s"""/metals.json
           |{
           |  "a": {
           |    "scalaVersion": "2.12.4",
           |    "libraryDependencies": ["com.typesafe.akka::akka-actor-typed:2.6.16"]
           |  }
           |}
        """.stripMargin
      )
      akkaLocations <- server.findTextInDependencyJars(
        include = ".conf",
        pattern = "jvm-shutdown-hooks"
      )
      jdkLocations <- server.findTextInDependencyJars(
        include = ".java",
        pattern = "public String(StringBuffer buffer) {"
      )
    } yield {

      assertLocations(
        akkaLocations,
        s"""|
            |akka-actor_2.12-${akkaVersion}.jar/reference.conf:96:3: info: result
            |  jvm-shutdown-hooks = on
            |  ^^^^^^^^^^^^^^^^^^
            |akka-actor_2.12-${akkaVersion}.jar/reference.conf:1178:41: info: result
            |    # This property is related to `akka.jvm-shutdown-hooks` above.
            |                                        ^^^^^^^^^^^^^^^^^^
            |akka-actor_2.12-${akkaVersion}-sources.jar/reference.conf:96:3: info: result
            |  jvm-shutdown-hooks = on
            |  ^^^^^^^^^^^^^^^^^^
            |akka-actor_2.12-${akkaVersion}-sources.jar/reference.conf:1178:41: info: result
            |    # This property is related to `akka.jvm-shutdown-hooks` above.
            |                                        ^^^^^^^^^^^^^^^^^^
            |""".stripMargin
      )

      assertLocations(
        jdkLocations, {
          val line =
            if (isJavaAtLeast17) 1444
            else if (isJavaAtLeast9) 626
            else 578

          val pathPrefix =
            if (isJavaAtLeast9) "/java.base/"
            else "/"

          s"""|src.zip${pathPrefix}java/lang/String.java:$line:5: info: result
              |    public String(StringBuffer buffer) {
              |    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
              |""".stripMargin
        }
      )
    }
  }

  private def assertLocations(
      locations: List[Location],
      expected: String
  ): Unit = {
    val rendered = locations
      .map { loc =>
        val path = AbsolutePath.fromAbsoluteUri(URI.create(loc.getUri()))
        val relativePath =
          path
            .toRelative(workspace.resolve(Directories.dependencies))
            .toString()
            .replace("\\", "/")

        val input = path.toInput.copy(path = relativePath.toString)
        loc
          .getRange()
          .toMeta(input)
          .formatMessage("info", "result")
      }
      .mkString("\n")
    assertNoDiff(rendered, expected)
  }
}
