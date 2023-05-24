package WayFinder;

import Scraper._
import Parser._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn.readLine
import java.io.File
import java.io.FileWriter
import scala.concurrent.Future

class WayFinder(val startUrl: String, val endUrl: String, val urlPrefix: String, val outputPath: FileWriter, val isLast: Boolean = false) {
    private val edgeMap = Map[String, List[String]]();
    private val distanceMap = Map[String, Int]();
    private val visited = Set[String]();
    def startBfs(): Unit = {
        val distanceUrls = Set(startUrl);
        bfs(startUrl, endUrl, distanceUrls, distanceMap, edgeMap, visited, 0);
    }
    def findWays(
        startUrl: String, 
        endUrl: String, 
        edgeMap: Map[String, List[String]], 
        visited: Set[String], 
        currentWay: List[String], 
        distance: Int, 
        maxDistance: Int):(Boolean, List[List[String]]) = {
            if(!edgeMap.contains(startUrl)) {
                return (false, List());
            }
            val toVisit = edgeMap(startUrl).filter(!visited.contains(_));
            if(distance > maxDistance) {
                return (false, List());
            }
            if(toVisit.contains(endUrl)) {
                return (true, List(currentWay :+ startUrl :+ endUrl));
            }
            val newVisited = visited ++ toVisit;
            val newDistance = distance + 1;
            val newCurrentWay = currentWay :+ startUrl;
            val ways = toVisit.map(findWays(_, endUrl, edgeMap, newVisited, newCurrentWay, newDistance, maxDistance));
            val waysFiltered = ways.filter(_._1);
            val waysFilteredFlattened = waysFiltered.flatMap(_._2);
            return (waysFilteredFlattened.nonEmpty, waysFilteredFlattened);
    }
    def bfs(
        startUrl: String, 
        endUrl: String, 
        distanceUrls: Set[String], 
        distanceMap: Map[String, Int], 
        edgeMap: Map[String, List[String]], 
        visited: Set[String], 
        distance: Int
        ): Unit = {
            println(distance)
                if(distanceUrls.contains(endUrl)) {
                    println("Found");
                    val ways = findWays(startUrl, endUrl, edgeMap, Set(), List(), 0, distance)._2;
                    for(way <- ways) {
                        outputPath.write("(");
                        for(url <- way) {
                            if(url != way.last)
                            outputPath.write(Parser.makeTitleFromLink(url) + " ,");
                            else
                            outputPath.write(Parser.makeTitleFromLink(url));
                        }
                        outputPath.write(")\n");
                    }
                    if(isLast) {
                        outputPath.close();
                    }
                    return;
                }
                if(distance > 3){
                    if(isLast) {
                        outputPath.write("No way\n");
                        outputPath.close();
                    }
                    return;
                }
                
            val distanceUrlsList = distanceUrls.toList;
            val promises = Scraper.getPagePromises(distanceUrlsList);
            val content = Scraper.getBodyPromises(promises);
            val inverseContent = Future.sequence(content);
            inverseContent onComplete {
                case scala.util.Success(value) => {
                    val valueChecked = value.filter(_.isRight).map(_.right.get);
                    val linkBodyTuple = distanceUrlsList.zip(valueChecked);
                    val (newMap, linkList) = Parser.GetLinksFromBodyList(linkBodyTuple, urlPrefix, edgeMap);
                    val newDistanceUrls = linkList.filter(!visited.contains(_));
                    val newDistanceMap = distanceMap ++ newDistanceUrls.map((_, distance));
                    val newVisited = visited ++ newDistanceUrls;
                    bfs(startUrl, endUrl, newDistanceUrls.toSet, newDistanceMap, newMap, newVisited, distance + 1);
                }
                case scala.util.Failure(exception) => {}//println(exception)
            }

    }
}