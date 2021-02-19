import sbt.Keys._
import sbt._

object BulkySourcesPlugin extends AutoPlugin {

  lazy val bulkySources = taskKey[Seq[(Int, File)]](
    "Return files that line of code more then 'bulkyThresholdInLines'"
  )
  lazy val bulkyThresholdInLines = settingKey[Int](
    "Min bulky threshold lines, files that have more lines is bulky. Default is 100."
  )

  override def projectSettings: Seq[Setting[_]] = Seq(
    bulkyThresholdInLines := 100,

    bulkySources := {
      val compileFiles = (sources in Compile).value
      val testCompileFiles = (sources in Test).value
      val result = compileFiles ++ testCompileFiles
      result.map { file =>
        val linesCount = sbt.IO.readLines(file).filterNot(_.isBlank).size
        (linesCount, file)
      }.filter { case (linesCount, _) => linesCount >= bulkyThresholdInLines.value }
    }
  )
}