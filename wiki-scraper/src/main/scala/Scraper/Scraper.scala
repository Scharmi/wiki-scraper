package Scraper;

import sttp.client4._
import sttp.client4.okhttp.OkHttpFutureBackend
import sttp.client4._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn.readLine
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._

object Scraper {
    private implicit val backend = OkHttpFutureBackend()
    private implicit val syncBackend = DefaultSyncBackend()
    def getPagePromises(urls: List[String], prefix: String = ""): List[Future[Response[Either[String, String]]]] = {
        def getPagePromisesHelper(urls: List[String], accumulator: List[Future[Response[Either[String, String]]]]): List[Future[Response[Either[String, String]]]] = {
            urls match {
            case Nil => accumulator
            case head :: tail =>
                val address = head match {
                case s if s.startsWith("http") => s
                case s => prefix + s
                }
                val request = basicRequest.get(uri"$address")
                val response = request.send(backend)
                getPagePromisesHelper(tail, response :: accumulator)
            }
        }
        getPagePromisesHelper(urls, List.empty)
    }
    def getBodyPromises(promises: List[Future[sttp.client4.Response[Either[String, String]]]]): List[Future[Either[String, String]]] = {
        def getBodyPromisesHelper(remainingPromises: List[Future[sttp.client4.Response[Either[String, String]]]], acc: List[Future[Either[String, String]]]): List[Future[Either[String, String]]] = {
            remainingPromises match {
            case Nil => acc
            case head :: tail =>
                val futureBody = head.map(_.body)
                getBodyPromisesHelper(tail, futureBody :: acc)
            }
        }

        getBodyPromisesHelper(promises, List()).reverse
    }
    def getPage(url: String): String = {
        val request = basicRequest.get(uri"$url")
        val response = request.send(backend)
        val body = response.map(_.body)
        val result = Await.result(body, Duration.Inf)
        result match {
            case Right(value) => value
            case Left(value) => value
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