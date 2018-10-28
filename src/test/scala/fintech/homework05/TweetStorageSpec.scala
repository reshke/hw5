

package fintech.homework05

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}

class TweetStorageSpec extends FlatSpec with Matchers {

  val tweet = Tweet("0",
    "1010100101010101",
    "Hello #there",
    Seq("#there"),
    Some(Instant.now), 0)


  val longTweet = Tweet("0",
    "1010100101010101",
    "Hello #there vierjgoiejriogjeiorgjioerfjoiwjfoiwrjfoiwejfiowejfoijwefoijoifjweoifjwoiefjoiwefjoiwejfoiwejfoiwejfoiwejfoiwjefoiwjefoijewfoijweoifjweoifjwoiefjiowejfoiwejfoiwefvierjgoiejriogjeiorgjioerfjoiwjfoiwrjfoiwejfiowejfoijwefoijoifjweoifjwoiefjoiwefjoiwejfoiwejfoiwejfoiwejfoiwjefoiwjefoijewfoijweoifjweoifjwoiefjiowejfoiwejfoiwef",
    Seq("#there"),
    Some(Instant.now), 0)

  it should "write tweet into storage correctly" in {
    val storage = new Storage()
    val res = storage.writeTweet(tweet)
    res.getResultState() should be(ResultState.Success)
    res.getResult().get should be(tweet)
  }

  it should "find tweet by id well" in {
    val storage = new Storage()
    storage.writeTweet(tweet)
    val res = storage.findTweet(tweet.id)
    res.getResultState() should be(ResultState.Success)
    res.getResult().get should be(tweet)
  }

  it should "delete tweet by id well" in {
    val storage = new Storage()
    storage.writeTweet(tweet)
    val res = storage.deleteTweet(tweet.id)
    res.getResultState() should be(ResultState.Success)
    res.getResult().get should be(tweet)

    val anotherRes = storage.deleteTweet(tweet.id)
    anotherRes.getResultState() should be(ResultState.Error)
    anotherRes.getErrorMessage() should be("there is no tweet with such id")
  }

  it should "do not find irrelevant" in {
    val storage = new Storage()
    storage.writeTweet(tweet)
    val res = storage.findTweet(tweet.id + "reigioerjgioerg")
    res.getResultState() should be(ResultState.Error)
    res.getErrorMessage() should be("Failed to find tweet : there is no tweet with such id")
  }

  it should "do not write the same tweet more than 1 time" in {
    val storage = new Storage()
    storage.writeTweet(tweet)

    val res = storage.writeTweet(tweet)

    res.getResultState() should be(ResultState.Error)
    res.getErrorMessage() should be("Failed to write a new tweet : incorrect id")
  }

  it should "not write too long tweet" in {
    val storage = new Storage()
    val res = storage.writeTweet(longTweet)
    res.getResultState() should be(ResultState.Error)
    res.getErrorMessage() should be("Tweet is too long")
  }
}

