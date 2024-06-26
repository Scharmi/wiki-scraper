import sttp.client4._ 
import sttp.client4.okhttp.OkHttpFutureBackend
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn.readLine
import scala.concurrent.Future
import scala.concurrent.Await
import scala.io.Source
import java.io.File
import java.io.FileWriter
import Scraper._
import Parser._
import WayFinder._

object Main extends App {
  val inputPath = args(0)
  val outputPath = args(1)
  //val inputPath= "/home/scharmi/Scala/wiki-scraper/src/main/scala/a.in";
  //val outputPath = "/home/scharmi/Scala/wiki-scraper/src/main/scala/a.out";
  val inputLines = Source.fromFile("/home/scharmi/Scala/wiki-scraper/src/main/scala/a.in").getLines.toList
  val inputLinesWithoutBrackets = inputLines.map(
    _ match {
      case s if s.startsWith("(") => s.substring(1, s.length - 1)
      case s => s
    }
  )
  val fileWriter = new FileWriter(new File(outputPath))
  val linesToList = inputLinesWithoutBrackets.map(_.split(", "));
  val resultFutures = for(line <- linesToList) {
    fileWriter.write("(")
    (new WayFinder(Parser.makeLinkFromTitle(line(1), line(0)), Parser.makeLinkFromTitle(line(2), line(0)), Parser.makePrefix(line(0)), fileWriter, linesToList.last == line)).dfsWrapper();
    fileWriter.write(")\n")
  }
  fileWriter.close()
}