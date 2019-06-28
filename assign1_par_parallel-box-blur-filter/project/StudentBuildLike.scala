import sbt._
import Keys._
import Settings._

import java.io.{File, IOException, FileInputStream}
import org.apache.commons.codec.binary.Base64

import scalaj.http._

/**
  * Note: keep this class concrete (i.e., do not convert it to abstract class or trait).
  */
class StudentBuildLike protected() extends CommonBuild {

  lazy val root = project.in(file(".")).settings(
    course := "",
    assignment := "",
    submitSetting,
    commonSourcePackages := Seq(), // see build.sbt
    courseId := "",
    styleCheckSetting,
    libraryDependencies += "ch.epfl.lamp" % "scala-grading-runtime_2.11" % "0.3"
  ).settings(packageSubmissionFiles: _*)


  //TODOs
  //- handle 400s coursera error codes

  /** **********************************************************
    * SUBMITTING A SOLUTION TO COURSERA
    */

  val packageSubmission = TaskKey[File]("packageSubmission")

  val packageSubmissionFiles = {
    // the packageSrc task uses Defaults.packageSrcMappings, which is defined as concatMappings(resourceMappings, sourceMappings)
    // in the packageSubmission task we only use the sources, not the resources.
    inConfig(Compile)(Defaults.packageTaskSettings(packageSubmission, Defaults.sourceMappings))
  }

  /** Task to submit a solution to coursera */
  val submit = inputKey[Unit]("submit")

  lazy val submitSetting = submit := {
    val args: Seq[String] = Def.spaceDelimited("<arg>").parsed
    val (_, _) = (clean.value, (compile in Compile).value) // depends on clean & compile task
    val s: TaskStreams = streams.value // for logging
    val jar = (packageSubmission in Compile).value

    val (email, secret) = args match {
      case email :: secret :: Nil =>
        (email, secret)
      case _ =>
        val inputErr =
          s"""|Invalid input to `submit`. The required syntax for `submit` is:
              |submit <email-address> <submit-token>
              |
              |The submit token is NOT YOUR LOGIN PASSWORD. It can be obtained from the assignment page.
          """.stripMargin
        s.log.error(inputErr)
        failSubmit()
    }

    /** Check that the jar exists, isn't empty, isn't crazy big, and can be read
      * If so, encode jar as base64 so we can send it to Coursera
      */
    def prepareJar(jar: File): String = {
      val errPrefix = "Error submitting assignment jar: "
      val fileLength = jar.length()
      if (!jar.exists()) {
        s.log.error(errPrefix + "jar archive does not exist\n" + jar.getAbsolutePath)
        failSubmit()
      } else if (fileLength == 0L) {
        s.log.error(errPrefix + "jar archive is empty\n" + jar.getAbsolutePath)
        failSubmit()
      } else if (fileLength > maxSubmitFileSize) {
        s.log.error(errPrefix + "jar archive is too big. Allowed size: " +
          maxSubmitFileSize + " bytes, found " + fileLength + " bytes.\n" +
          jar.getAbsolutePath)
        failSubmit()
      } else {
        val bytes = new Array[Byte](fileLength.toInt)
        val sizeRead = try {
          val is = new FileInputStream(jar)
          val read = is.read(bytes)
          is.close()
          read
        } catch {
          case ex: IOException =>
            s.log.error(errPrefix + "failed to read sources jar archive\n" + ex.toString)
            failSubmit()
        }
        if (sizeRead != bytes.length) {
          s.log.error(errPrefix + "failed to read the sources jar archive, size read: " + sizeRead)
          failSubmit()
        } else encodeBase64(bytes)
      }
    }

    val assignmentName = assignment.value
    val assignmentDetails = assignmentsMap.value(assignmentName)
    val assignmentKey = assignmentDetails.key
    val courseName = course.value
    val partId = assignmentDetails.partId
    val base64Jar = prepareJar(jar)
    val json =
      s"""|{
          |   "assignmentKey":"$assignmentKey",
          |   "submitterEmail":"$email",
          |   "secret":"$secret",
          |   "parts":{
          |      "$partId":{
          |         "output":"$base64Jar"
          |      }
          |   }
          |}""".stripMargin

    def postSubmission[T](data: String): HttpResponse[String] = {
      val http = Http("https://www.coursera.org/api/onDemandProgrammingScriptSubmissions.v1")
      val hs = List(
        ("Cache-Control", "no-cache"),
        ("Content-Type", "application/json")
      )
      val response = http.postData(data).headers(hs).asString // kick off HTTP POST
      response
    }

    val connectMsg =
      s"""|Connecting to Coursera...
          |Attempting to submit "$assignmentName" assignment in "$courseName" course
          |Using:
          |- email: $email
          |- submit token: $secret""".stripMargin
    s.log.info(connectMsg)
    postSubmission(json) // kick it all off
  }

  def failSubmit(): Nothing = {
    sys.error("Submission failed")
  }

  /**
    * *****************
    * DEALING WITH JARS
    */

  def encodeBase64(bytes: Array[Byte]): String =
    new String(Base64.encodeBase64(bytes))

  def decodeBase64(str: String): Array[Byte] = {
    // codecs 1.4 has a version accepting a string, but not 1.2; jar hell.
    Base64.decodeBase64(str.getBytes)
  }

  /** *****************************************************************
    * RUNNING WEIGHTED SCALATEST & STYLE CHECKER ON DEVELOPMENT SOURCES
    */

  val styleCheck = TaskKey[Unit]("styleCheck")
  val styleCheckSetting = styleCheck := {
    val (_, sourceFiles, assignments, assignmentName) = ((compile in Compile).value, (sources in Compile).value, assignmentsMap.value, assignment.value)
    val styleSheet = assignments(assignmentName).styleSheet
    val logger = streams.value.log
    if (styleSheet != "") {
      val (feedback, score) = StyleChecker.assess(sourceFiles, styleSheet)
      logger.info(
        s"""|$feedback
            |Style Score: $score out of ${StyleChecker.maxResult}""".stripMargin)
    } else logger.warn("Can't check style: there is no style sheet provided.")
  }

}
