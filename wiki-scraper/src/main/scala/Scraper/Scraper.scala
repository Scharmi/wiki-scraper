package Scraper;

import sttp.client4._
import sttp.client4.okhttp.OkHttpFutureBackend
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn.readLine
import scala.concurrent.Future
import scala.concurrent.Await
object Scraper {
    
    def getPagePromises(urls: List[String]): List[Future[sttp.client4.Response[Either[String,String]]]] = {
        urls match {
            case Nil => List()
            case head :: tail => {
                val request = basicRequest.get(uri"$head");
                implicit val backend = OkHttpFutureBackend()
                val response = request.send(backend);
                response :: getPagePromises(tail)
            }
        }
    }
    def getBodyPromises(promises: List[Future[sttp.client4.Response[Either[String,String]]]]): List[Future[Either[String,String]]] = {
        promises match {
            case Nil => List()
            case head :: tail => {
                head.map(_.body) :: getBodyPromises(tail)
            }
        }
    }

    def converToNilIfErrors(list: List[Either[String,String]]): Boolean = {
        list match {
            case Nil => true
            case head :: tail => {
                head match {
                    case Left(_) => false
                    case Right(_) => converToNilIfErrors(tail)
                }
            }
        }
    }
}