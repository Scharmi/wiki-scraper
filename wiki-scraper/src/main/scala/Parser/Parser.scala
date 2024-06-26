package Parser;

import scala.util.matching.Regex

object Parser {
    def linkFilter = (s: String) => {
        s match {
            case s if s.startsWith("/wiki") => true
            case _ => false    
        }

    }
    def getLinksFromBody(body: String, urlPrefix: String): List[String] = {
        val pattern = """<a href="([^"]*)""".r
        val links = pattern.findAllIn(body).matchData.map(_.group(1)).filter(linkFilter).toList
        links.map(urlPrefix + _)
    }
    def GetLinksFromBodyList(
        bodies: List[(String, String)], 
        urlPrefix: String, 
        mapToUpdate: Map[String, List[String]]
    ): (Map[String, List[String]], List[String]) = {
        def processTailRec(
            bodies: List[(String, String)], 
            urlPrefix: String, 
            acc: (Map[String, List[String]], List[String])
        ): (Map[String, List[String]], List[String]) = {
            bodies match {
            case Nil => acc
            case head :: tail =>
                val updatedMap = acc._1 + (head._1 -> (acc._1.getOrElse(head._1, Nil) ++ getLinksFromBody(head._2, urlPrefix)))
                processTailRec(tail, urlPrefix, (updatedMap, acc._2 ++ getLinksFromBody(head._2, urlPrefix)))
            }
        }
        processTailRec(bodies, urlPrefix, (mapToUpdate, List.empty))
    }
    def makeLinkFromTitle(title: String, lang: String): String = {
        "https://" + lang + ".wikipedia.org/wiki/" + title.replaceAll(" ", "_")
    }
    def makePrefix(lang: String) = {
        "https://" + lang + ".wikipedia.org"
    }
    def makeTitleFromLink(link: String): String = {
        link.split("/").last.replaceAll("_", " ")
    }
}