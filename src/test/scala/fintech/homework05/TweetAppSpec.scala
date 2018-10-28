package fintech.homework05
import org.scalatest.{FlatSpec, Matchers}

class TweetAppSpec extends FlatSpec with Matchers {
  val storage = new Storage
  val app = new TweetApi(storage)

  it should "create tweet well" in {
    val request = CreateTweetRequest("hello #there", "reshke")
    val tweet = app.createTweet(request).getResult().get

    tweet should be(Tweet(tweet.id, "reshke", "hello #there", Seq("#there"), tweet.createdAt, 0))
  }

  it should "create tweet with correct hashTags" in {
    val request = CreateTweetRequest("Hello #there #u #how #wav #prev", "some name")
    val tweet = app.createTweet(request)

    tweet.getResult().get.hashTags should be(Seq("#there", "#u", "#how", "#wav", "#prev"))
  }

  it should "like tweet well" in {
    val request = CreateTweetRequest("hello #there", "reshke")

    val tweet = app.likeTweet(LikeRequest(app.createTweet(request).getResult().get.id)).getResult().get

    tweet should be(Tweet(tweet.id, "reshke", "hello #there", Seq("#there"), tweet.createdAt, 1))
  }

  it should "get tweet well" in {
    val request = CreateTweetRequest("hello #there", "reshke")
    val res = app.createTweet(request).getResult().get

    val tweet = app.getTweet(GetTweetRequest(res.id)).getResult().get
    tweet should be(res)
  }


}
