package fintech.homework05
import org.scalatest.{FlatSpec, Matchers}

class TweetAppSpec extends FlatSpec with Matchers {
  val storage = new Storage
  val app = new TweetApi(storage)

  it should "create tweet well" in {
    val request = CreateTweetRequest("hello #there", "reshke")
    val res = app.createTweet(request)

    res match {
      case Error(errorMessage) => throw new AssertionError(errorMessage)
      case Success(value) =>
        val tweet = value.get
        tweet should be(Tweet(tweet.id, "reshke", "hello #there", Seq("#there"), tweet.createdAt, 0))
    }
  }

  it should "create tweet with correct hashTags" in {
    val request = CreateTweetRequest("Hello #there #u #how #wav #prev", "some name")
    val res = app.createTweet(request)

    res match {
      case Error(errorMessage) => throw new AssertionError(errorMessage)
      case Success(value) =>
        val tweet = value.get
        tweet.hashTags should be(Seq("#there", "#u", "#how", "#wav", "#prev"))
    }
  }

  it should "like tweet well" in {
    val createRequest = CreateTweetRequest("hello #there", "reshke")
    val res = app.createTweet(createRequest)

    res match {
      case Error(errorMessage) => throw new AssertionError(errorMessage)
      case Success(value) =>
        val tweet = value.get
        val id = tweet.id
        val likeRequest = LikeRequest(id)


        val result = app.likeTweet(likeRequest)

        result match {
          case Error(errorMessage) => throw new AssertionError(errorMessage)
          case Success(value) =>
            val tweet = value.get

            tweet should be(Tweet(id, "reshke", "hello #there", Seq("#there"), tweet.createdAt, 1))
        }
    }

  }

  it should "get tweet well" in {
    val request = CreateTweetRequest("hello #there", "reshke")
    val res = app.createTweet(request)

    res match {
      case Error(errorMessage) => throw new AssertionError(errorMessage)
      case Success(value) =>
        val tweet = value.get
        val id = tweet.id
        val result = app.getTweet(GetTweetRequest(id))

        result match {
          case Error(errorMessage) => throw new AssertionError(errorMessage)
          case Success(value) =>
            value.get should be(tweet)
        }
    }
  }
}