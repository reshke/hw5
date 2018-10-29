

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
    "Hello #there vierjgoiejriogjeiorgjioerfjoiwjfoiwrjfIAMTHECLITCOMANDERowejfoijwefoijoifjweoifjwoiefПАНКИХОЙРЕПОТСТОЙjoiwejfoiwejfoiwejfoiwejfoiwjefoiwjefoijewfoijweoifjweoifjwoiefjiowejfoiwejfoiwefvierjgoiejriogjeiorgjioerfjoiwjfoiwrjfoiwejfiowejfoijwefoijoifjweoifjwoiefjoiwefjoiwejfoiwejfoiwejfoiwejfoiwjefoiwjefoijewfoijweoifjweoifjwoiefjiowejfoiwejfoiwef",
    Seq("#there"),
    Some(Instant.now), 0)

  it should "write tweet into storage correctly" in {
    val storage = new Storage()
    val res = storage.writeTweet(tweet)
    res match {
      case Error(errorMessage) => throw new AssertionError(errorMessage)
      case Success(result)=>
        result.get should be (tweet)
    }
  }

  it should "find tweet by id well" in {
    val storage = new Storage()
    storage.writeTweet(tweet)
    val res = storage.findTweet(tweet.id)

    res match {
      case Error(errorMessage) => throw new AssertionError(errorMessage)
      case Success(result)=>
        result.get should be (tweet)
    }
  }

  it should "delete tweet by id well" in {
    val storage = new Storage()
    storage.writeTweet(tweet)
    val res = storage.deleteTweet(tweet.id)

    res match {
      case Error(errorMessage) => throw new AssertionError(errorMessage)
      case Success(result)=>
        result.get should be (tweet)
    }

    val anotherRes = storage.deleteTweet(tweet.id)

    anotherRes match {
      case Error(errorMessage) => errorMessage should be("there is no tweet with such id")
      case Success(_)=>
        throw new AssertionError("Tweet was not deleted")
    }
  }

  it should "do not find irrelevant" in {
    val storage = new Storage()
    storage.writeTweet(tweet)
    val res = storage.findTweet(tweet.id + "reigioerjgioerg")
    res match {
      case Error(errorMessage) => errorMessage should be("Failed to find tweet : there is no tweet with such id")
      case Success(_) =>
        throw new AssertionError("Nonexistent tweet was found")
    }
  }

  it should "do not write the same tweet more than 1 time" in {
    val storage = new Storage()
    storage.writeTweet(tweet)

    val res = storage.writeTweet(tweet)

    res match {
      case Error(errorMessage) => errorMessage should be("Failed to write a new tweet : incorrect id")
      case Success(_) =>
        throw new AssertionError("Tweet was saved 2 times")
    }
  }

  it should "not write too long tweet" in {
    val storage = new Storage()
    val res = storage.writeTweet(longTweet)

    res match {
      case Error(errorMessage) => errorMessage should be("Tweet is too long")
      case Success(_) =>
        throw new AssertionError("Too long tweet saved")
    }
  }
}

