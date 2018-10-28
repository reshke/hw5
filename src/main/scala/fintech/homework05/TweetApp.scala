package fintech.homework05

import java.time.Instant
import java.util.UUID
import scala.util.matching.Regex

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Result[T]
  * в котором может лежать либо текст ошибки, либо результат выполнение
  */

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int)

case class CreateTweetRequest(text: String, user: String)
case class GetTweetRequest(id: String)
case class LikeRequest(id: String)

object ResultState extends Enumeration {
  type ResultState = Value
  val Error, Success = Value
}

import ResultState._

sealed trait Result[T] {
  def getResultState(): ResultState

  def getErrorMessage(): String

  def getResult(): Option[T]
}

case class TweetInfo(value : ResultState)
                    (result : Option[Tweet] = Option.empty[Tweet])
                    (errorMessage : String = "No error occurred") extends Result[Tweet] {
  def getResultState(): ResultState = value

  def getErrorMessage(): String = errorMessage

  def getResult(): Option[Tweet] = result
}

trait TweetStorage {
  def writeTweet(tweet: Tweet): Result[Tweet]

  def deleteTweet(id: String): Result[Tweet]

  def findTweet(id: String): Result[Tweet]
}

class Storage(var map: Map[String, Tweet] = Map.empty) extends TweetStorage {
  override def writeTweet(tweet: Tweet): Result[Tweet] = {
    if (map contains tweet.id) {
      TweetInfo(Error)()("Failed to write a new tweet : incorrect id")
    }
    else {
      if (tweet.text.length > 280) {
        TweetInfo(Error)()("Tweet is too long")
      }
      else {
        map += (tweet.id -> tweet)
        TweetInfo(Success)(Some(tweet))()
      }
    }
  }

  override def deleteTweet(id: String): Result[Tweet] = {
    val tweet = map.get(id)
    if (tweet.isDefined) {
      map -= id
      TweetInfo(Success)(tweet)()
    }
    else {
      TweetInfo(Error)(tweet)(errorMessage = "there is no tweet with such id")
    }
  }

  override def findTweet(id: String): Result[Tweet] = {
    val tweet = map.get(id)
    if (tweet.isDefined)
      TweetInfo(Success)(tweet)()
    else
      TweetInfo(Error)()("Failed to find tweet : there is no tweet with such id")
  }
}

class TweetApi(storage: TweetStorage) {
  def createTweet(request: CreateTweetRequest): Result[Tweet] = {
    val text = request.text
    val user = request.user

    val reg : Regex = "#[0-9A-Za-z]+".r

    val hashTags = for (hashTag <- reg.findAllMatchIn(text)) yield hashTag.toString()
    var seq : Seq[String] = Seq.empty[String]

    for (each <- hashTags)
      seq = seq :+ each

    TweetInfo(Success)(storage.writeTweet(Tweet(UUID.randomUUID.toString,
      user, text, seq, Some(Instant.now()), 0)) getResult())()
  }

  def getTweet(request: GetTweetRequest): Result[Tweet] = {
    val res = storage.findTweet(request.id)

    if (res.getResultState == Error)
      TweetInfo(Error)()(res.getErrorMessage())
    else
      TweetInfo(Success)(res.getResult())()
  }

  def likeTweet(request: LikeRequest): Result[Tweet] = {
    val res = storage.findTweet(request.id)
    res.getResultState() match {
      case Error => res
      case Success =>
        val info = storage.deleteTweet(request.id)
        info.getResultState() match {
          case Error => TweetInfo(Error)()("impossible to like this tweet : there is no tweet with such id")
          case Success =>
            val tweet = info.getResult().get
            storage.writeTweet(Tweet(tweet.id, tweet.user, tweet.text, tweet.hashTags, tweet.createdAt, tweet.likes + 1))
        }
    }
  }
}


object TweetApiExample extends App {

  val storage: TweetStorage = new Storage()
  val app = new TweetApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val response = app.createTweet(request)
  response.getResultState() match {
    case Success =>
      val id = response.getResult().get.id
      println(s"Created tweet with id: $id")
    case Error =>
      val message = response.getErrorMessage()
      println(s"Failed to create tweet:  $message")
  }
}
