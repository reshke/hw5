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

sealed trait Result[T] {
}

case class Error(errorMessage : String = "Some errors ocupped") extends Result[Tweet]{
  def getErrorMessage() : String = errorMessage
}

case class Success(result : Option[Tweet] = Option.empty[Tweet]) extends Result[Tweet] {
  def getResult(): Option[Tweet] = result
}

trait TweetStorage {
  def writeTweet(tweet: Tweet): Result[Tweet]

  def deleteTweet(id: String): Result[Tweet]

  def findTweet(id: String): Result[Tweet]
}

class Storage(private var map: Map[String, Tweet] = Map.empty) extends TweetStorage {
  override def writeTweet(tweet: Tweet): Result[Tweet] = {
    if (map contains tweet.id) {
      Error("Failed to write a new tweet : incorrect id")
    }
    else {
      if (tweet.text.length > 280) {
        Error("Tweet is too long")
      }
      else {
        map += (tweet.id -> tweet)
        Success(Some(tweet))
      }
    }
  }

  override def deleteTweet(id: String): Result[Tweet] = {
    val tweet = map.get(id)
    if (tweet.isDefined) {
      map -= id
      Success(tweet)
    }
    else {
      Error(errorMessage = "there is no tweet with such id")
    }
  }

  override def findTweet(id: String): Result[Tweet] = {
    val tweet = map.get(id)
    if (tweet.isDefined)
      Success(tweet)
    else
      Error("Failed to find tweet : there is no tweet with such id")
  }
}

class TweetApi(storage: TweetStorage) {
  def createTweet(request: CreateTweetRequest): Result[Tweet] = {
    val text = request.text
    val user = request.user

    val reg: Regex = "#[0-9A-Za-z]+".r

    val hashTags = (for (hashTag <- reg.findAllMatchIn(text)) yield hashTag.toString()).toSeq
    storage.writeTweet(Tweet(UUID.randomUUID.toString, user, text, hashTags, Some(Instant.now()), 0))
  }

  def getTweet(request: GetTweetRequest): Result[Tweet] = {
    val res = storage.findTweet(request.id)

    res
  }

  def likeTweet(request: LikeRequest): Result[Tweet] = {
    val res = storage.findTweet(request.id)
    res match {
      case Error(_) => res
      case Success(_) =>
        val info = storage.deleteTweet(request.id)
        info match {
          case Error(_) => Error("impossible to like this tweet : there is no tweet with such id")
          case Success(result) =>
            val tweet = result.get
            storage.writeTweet(tweet.copy(likes = tweet.likes + 1))
        }
    }
  }
}


object TweetApiExample extends App {

  val storage: TweetStorage = new Storage()
  val app = new TweetApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val response = app.createTweet(request)
  response match {
    case Success(value) =>
      val id = value.get.id
      println(s"Created tweet with id: $id")
    case Error(message) =>
      println(s"Failed to create tweet:  $message")
  }
}
