package WayFinder;

import Scraper._
import Parser._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn.readLine
import java.io.File
import java.io.FileWriter
import scala.io.Source
import java.io.File
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._


class WayFinder(val startUrl: String, val endUrl: String, val urlPrefix: String, val outputPath: FileWriter, val isLast: Boolean = false) {
    private val edgeMap = Map[String, List[String]]();
    private val distanceMap = Map[String, Int]();
    private val visited = Set[String]();
    private var completedRequests = 0;
    def dfsWrapper(maxDepth: Int = 0): Unit = {
        dfs(startUrl, endUrl, List(startUrl), 0, maxDepth, outputPath, urlPrefix) match {
            case false => 
                dfsWrapper(maxDepth + 1)
            case true =>
        }
    }

    def dfs(
        startUrl: String,
        endUrl: String,
        currentPath: List[String],
        currentDepth: Int,
        maxDepth: Int,
        outputPath: FileWriter,
        urlPrefix: String
    ): Boolean = {
    if (startUrl == endUrl) {
        outputPath.write(currentPath.map(Parser.makeTitleFromLink(_)).mkString("(", ", ", ") "))
        true
    } else if (currentDepth >= maxDepth) {
        false
    } else {
        val toVisit = Parser.getLinksFromBody(Scraper.getPage(startUrl), urlPrefix)
        val toVisitFiltered = toVisit.toSet.toList
        var found = false
        for(url <- toVisitFiltered) {
            if(url != startUrl) {
                if (dfs(url, endUrl, currentPath :+ url, currentDepth + 1, maxDepth, outputPath, urlPrefix)) {
                    found = true
                }
            }
        }
        found
    }
    }
}