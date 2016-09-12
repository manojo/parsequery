package parsequery

import util._

import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import org.scalameter.Key

import parsec._
import parsec.optimised._

object JSONData {
  val files = List("tweets-9700.json")

  implicit val range: Gen[List[String]] =
    Gen.enumeration("files")(files)
}

object JSONBenchmark extends Bench.Group {

  performance of "FastParseJSON" in {
    include (new FastParseJSONBenchmark {})
  }

  performance of "ParsequeryJSON" in {
    include (new ParsequeryJSONBenchmark {})
  }
}


//scalastyle:off line.size.limit
/**
 * The parser borrowed from FastParse
 * https://github.com/lihaoyi/fastparse/blob/master/fastparse/shared/src/test/scala/fastparse/JsonTests.scala
 */
//scalastyle:on line.size.limit

trait FastParseJSONBenchmark extends FastParseBenchmarkHelper {
  import JSONData._
  import fastparse.all._

  sealed abstract class JSValue
  case class JSObject(dict: List[(String, JSValue)]) extends JSValue
  case class JSArray(arr: List[JSValue]) extends JSValue
  case class JSDouble(d: Double) extends JSValue
  case class JSString(s: String) extends JSValue
  case class JSBool(b: Boolean) extends JSValue
  case object JSNull extends JSValue

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T): V = f(t)
    override def toString(): String = name
  }
  val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
  val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  val space = P(CharsWhile(Whitespace).?)
  val digits = P(CharsWhile(Digits))
  val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
  val fractional = P("." ~ digits)
  val integral = P("0" | CharIn('1' to '9') ~ digits.?)

  val number = P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
    x => JSDouble(x.toDouble)
  )

  val `null` = P("null").map(_ => JSNull)
  val `false` = P("false").map(_ => JSBool(false))
  val `true` = P("true").map(_ => JSBool(true))

  val hexDigit = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  val escape = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))

  val strChars = P(CharsWhile(StringChars))
  val string =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(JSString)

  val array =
    P("[" ~/ jsonExpr.rep(sep = ",".~/) ~ space ~ "]").map(xs => JSArray(xs.toList))

  val pair: P[(String, JSValue)] = P(string.map(_.s) ~/ ":" ~/ jsonExpr)

  val obj =
    P("{" ~/ pair.rep(sep = ",".~/) ~ space ~ "}").map(xs => JSObject(xs.toList))

  val jsonExpr: P[JSValue] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "tweets", jsonExpr)
  }(range)

}

/**
 * uses the `optimise` macro to partially evaluate the parser away
 * uses a general json parser rather then a dedicated parser
 */
trait ParsequeryJSONBenchmark extends ParsequeryBenchmarkHelper {
  import JSONData._

  sealed abstract class JSValue
  case class JSObject(dict: List[(String, JSValue)]) extends JSValue
  case class JSArray(arr: List[JSValue]) extends JSValue
  case class JSDouble(d: Double) extends JSValue
  case class JSString(s: String) extends JSValue
  case class JSBool(b: Boolean) extends JSValue
  case object JSNull extends JSValue

  val jsonParser = optimise {

    def main: Parser[JSValue] = (
      obj |
      arr |
      stringLiteral.map(x => JSString(x)) |
      double.map(x => JSDouble(x)) |
      accept("null").map(_ => JSNull) |
      accept("true").map(_ => JSBool(true)) |
      accept("false").map(_ => JSBool(false))
    )

    def obj: Parser[JSValue] = (skipWs(accept('{')) ~>
      repsep(member, skipWs(accept(',')))
    <~ skipWs(accept('}'))) map { x => JSObject(x) }

    def arr: Parser[JSValue] = (skipWs(accept('[')) ~>
      repsep(main, skipWs(accept(',')))
    <~ skipWs(accept(']'))) map { x => JSArray(x) }

    def member: Parser[(String, JSValue)] =
      stringLiteral ~ (skipWs(accept(':')) ~> main)

    main
  }

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "tweets", jsonParser)
  }(range)
}

/**
 * uses the `optimise` macro to partially evaluate the parser away
 * uses a general json parser rather then a dedicated parser
 *
 * This is tweet parser that follows most of the structure
 * as specified by the official Twitter docs. When we do choose
 * to ignore a structure we revert to a generic JSON parser
 */
trait ParsequeryTweetBenchmark extends ParsequeryBenchmarkHelper {
  import JSONData._

  sealed abstract class JSValue
  case class JSObject(dict: List[(String, JSValue)]) extends JSValue
  case class JSArray(arr: List[JSValue]) extends JSValue
  case class JSDouble(d: Double) extends JSValue
  case class JSString(s: String) extends JSValue
  case class JSBool(b: Boolean) extends JSValue
  case object JSNull extends JSValue

  val jsonParser = optimise {

    def main: Parser[JSValue] = (
      obj |
      arr |
      stringLiteral.map(x => JSString(x)) |
      double.map(x => JSDouble(x)) |
      accept("null").map(_ => JSNull) |
      accept("true").map(_ => JSBool(true)) |
      accept("false").map(_ => JSBool(false))
    )

    def obj: Parser[JSValue] = (skipWs(accept('{')) ~>
      repsep(member, skipWs(accept(',')))
    <~ skipWs(accept('}'))) map { x => JSObject(x) }

    def arr: Parser[JSValue] = (skipWs(accept('[')) ~>
      repsep(main, skipWs(accept(',')))
    <~ skipWs(accept(']'))) map { x => JSArray(x) }

    def member: Parser[(String, JSValue)] =
      stringLiteral ~ (skipWs(accept(':')) ~> main)

    main
  }

  case class Tweet(
    createdAt: String,
    id: Long,
    idString: String,
    text: String,
    source: String,
    truncated: Boolean,
    inReplyToStatusId: Option[Long],
    inReplyToStatusIdString: Option[String],
    inReplyToUserId: Option[Long],
    inReplyToUserIdString: Option[String],
    inReplyToScreenName: Option[String],
    user: User,
    //geo: ignore
    //coordinates: Option[Coordinates],
    //place: Option[Place],
    //contributors: List[Contributor],
    retweetCount: Int,
    favoriteCount: Int, //nullable aka == 0
    //entities: Entity, //ignore for now
    favorited: Option[Boolean] = None, //nullable aka == false
    retweeted: Option[Boolean] = None,
    possiblySensitive: Option[Boolean] = None,
    lang: String
  )

  case class User(
    id: Long,
    idString: String,
    name: String,
    screenName: String,
    location: Option[String] = None,
    description: Option[String] = None,
    url: Option[String] = None,
    //entities: Entity, ignore for now
    _protected: Boolean,
    followersCount: Int,
    friendsCount: Int,
    listedCount: Int,
    createdAt: String,
    favouritesCount: Int,
    utcOffset: Option[Int],
    timeZone: Option[String],
    geoEnabled: Boolean,
    verified: Boolean,
    statusesCount: Int,
    lang: String,
    contributorsEnabled: Boolean,
    isTranslator: Boolean,
    profileBgColor: String,
    profileBgImageUrl: String,
    profileBgImageUrlHttps: String,
    profileBgTile: Boolean,
    profileImageUrl: String,
    profileImageUrlHttps: String,
    profileBannerUrl: Option[String],
    profileLinkColor: String,
    profileSidebarBorderColor: String,
    profileSidebarFillColor: String,
    profileTextColor: String,
    profileUseBgImage: Boolean,
    defaultProfile: Boolean,
    defaultProfileImage: Boolean,
    followRequestSent: Option[Boolean]
  )

  def bool: Parser[Boolean] = {
    accept("true").map(_ => true) |
    accept("false").map(_ => false)
  }

  def nullOrBool: Parser[Option[Boolean]] = {
    accept("null").map(_ => None) |
    bool.map(b => Some(b))
  }

  def nullOrStrlit: Parser[Option[String]] = {
    accept("null").map(_ => None) |
    stringLiteral.map(s => Some(s))
  }

  def nullOrInt: Parser[Option[Int]] = {
    accept("null").map(_ => None) |
    negNumber.map(s => Some(s))
  }

  def nullOrLong: Parser[Option[Long]] = {
    accept("null").map(_ => None) |
    inaccurateLongParser.map(s => Some(s.toLong))
  }

  def inaccurateLongParser: Parser[Long] = (opt(accept('-')) ~>
    number.map(_.toLong) <~
    (opt(accept('.') ~> number ~> opt(accept("e+") ~> number)))
  )


  /***********************
   * UserParser
   ***********************/
  def idParser = accept("\"id\"") ~> skipWs(accept(':')) ~> inaccurateLongParser
  def idStrParser = accept("\"id_str\"") ~> skipWs(accept(':')) ~> stringLiteral
  def nameParser = accept("\"name\"") ~> skipWs(accept(':')) ~> stringLiteral
  def screenNameParser = accept("\"screen_name\"") ~> skipWs(accept(':')) ~> stringLiteral
  def locationParser =
    accept("\"location\"") ~> skipWs(accept(':')) ~> nullOrStrlit
  def descriptionParser =
    accept("\"description\"") ~> skipWs(accept(':')) ~> nullOrStrlit
  def urlParser = accept("\"url\"") ~> skipWs(accept(':')) ~> nullOrStrlit
  def entityParser = accept("\"entities\"") ~> skipWs(accept(':')) ~> jsonParser map (_ => ())
  def protectedParser = accept("\"protected\"") ~> skipWs(accept(':')) ~> bool
  def followersCountParser = accept("\"followers_count\"") ~> skipWs(accept(':')) ~> number
  def friendsCountParser = accept("\"friends_count\"") ~> skipWs(accept(':')) ~> number
  def listedCountParser = accept("\"listed_count\"") ~> skipWs(accept(':')) ~> number
  def createdAtParser = accept("\"created_at\"") ~> skipWs(accept(':')) ~> stringLiteral
  def favouritesCountParser = accept("\"favourites_count\"") ~> skipWs(accept(':')) ~> number
  def utcOffsetParser =
    accept("\"utc_offset\"") ~> skipWs(accept(':')) ~> nullOrInt
  def timeZoneParser =
    accept("\"time_zone\"") ~> skipWs(accept(':')) ~> nullOrStrlit
  def geoEnabledParser = accept("\"geo_enabled\"") ~> skipWs(accept(':')) ~> bool
  def verifiedParser = accept("\"verified\"") ~> skipWs(accept(':')) ~> bool
  def statusesCountParser = accept("\"statuses_count\"") ~> skipWs(accept(':')) ~> number
  def langParser = accept("\"lang\"") ~> skipWs(accept(':')) ~> stringLiteral
  def contribEnabledParser = accept("\"contributors_enabled\"") ~> skipWs(accept(':')) ~> bool
  def isTranslatorParser =  accept("\"is_translator\"") ~> skipWs(accept(':')) ~> bool
  def profBgColorParser =
    accept("\"profile_background_color\"") ~> skipWs(accept(':')) ~> stringLiteral
  def profBgImageUrlParser =
    accept("\"profile_background_image_url\"") ~> skipWs(accept(':')) ~> stringLiteral
  def profBgImageUrlHttpsParser =
    accept("\"profile_background_image_url_https\"") ~> skipWs(accept(':')) ~> stringLiteral
  def profBgTitleParser = accept("\"profile_background_tile\"") ~> skipWs(accept(':')) ~> bool
  def profImageUrlParser = accept("\"profile_image_url\"") ~> skipWs(accept(':')) ~> stringLiteral
  def profImageUrlHttpsParser =
    accept("\"profile_image_url_https\"") ~> skipWs(accept(':')) ~> stringLiteral
  def profileBannerUrlParser =
    accept("\"profile_banner_url\"") ~> skipWs(accept(':')) ~> stringLiteral
  def profLinkColorParser = accept("\"profile_link_color\"") ~> skipWs(accept(':')) ~> stringLiteral
  def profSidebarBorderColorParser =
    accept("\"profile_sidebar_border_color\"") ~> skipWs(accept(':')) ~> stringLiteral
  def profSidebarFillColorParser =
    accept("\"profile_sidebar_fill_color\"") ~> skipWs(accept(':')) ~> stringLiteral
  def profTextColorParser = accept("\"profile_text_color\"") ~> skipWs(accept(':')) ~> stringLiteral
  def profUseBgImageParser =
    accept("\"profile_use_background_image\"") ~> skipWs(accept(':')) ~> bool
  def defaultProfileParser = accept("\"default_profile\"") ~> skipWs(accept(':')) ~> bool
  def defaultProfileImageParser =
    accept("\"default_profile_image\"") ~> skipWs(accept(':')) ~> bool
  def followingParser = accept("\"following\"") ~> skipWs(accept(':')) ~> bool
  def followRequestSentParser =
    accept("\"follow_request_sent\"") ~> skipWs(accept(':')) ~> bool.map(l => Some(l))
  def notificationsParser = accept("\"notifications\"") ~> skipWs(accept(':')) ~> bool

  def userParser: Parser[User] = (
    (accept("\"user\"") ~> skipWs(accept(':')) ~> skipWs(accept('{'))) ~>
    (idParser <~ skipWs(accept(','))) ~
    (idStrParser <~ skipWs(accept(','))) ~
    (nameParser <~ skipWs(accept(','))) ~
    (screenNameParser <~ skipWs(accept(','))) ~
    (locationParser <~ skipWs(accept(','))) ~
    (descriptionParser <~ skipWs(accept(','))) ~
    (urlParser <~ skipWs(accept(','))) ~
    ((entityParser <~ skipWs(accept(','))) ~>
    (protectedParser <~ skipWs(accept(',')))) ~
    (followersCountParser <~ skipWs(accept(','))) ~
    (friendsCountParser <~ skipWs(accept(','))) ~
    (listedCountParser <~ skipWs(accept(','))) ~
    (createdAtParser <~ skipWs(accept(','))) ~
    (favouritesCountParser <~ skipWs(accept(','))) ~
    (utcOffsetParser <~ skipWs(accept(','))) ~
    (timeZoneParser <~ skipWs(accept(','))) ~
    (geoEnabledParser <~ skipWs(accept(','))) ~
    (verifiedParser <~ skipWs(accept(','))) ~
    (statusesCountParser <~ skipWs(accept(','))) ~
    (langParser <~ skipWs(accept(','))) ~
    (contribEnabledParser <~ skipWs(accept(','))) ~
    (isTranslatorParser <~ skipWs(accept(','))) ~
    (profBgColorParser <~ skipWs(accept(','))) ~
    (profBgImageUrlParser <~ skipWs(accept(','))) ~
    (profBgImageUrlHttpsParser <~ skipWs(accept(','))) ~
    (profBgTitleParser <~ skipWs(accept(','))) ~
    (profImageUrlParser <~ skipWs(accept(','))) ~
    (profImageUrlHttpsParser <~ skipWs(accept(','))) ~
    opt(profileBannerUrlParser <~ skipWs(accept(','))) ~
    (profLinkColorParser <~ skipWs(accept(','))) ~
    (profSidebarBorderColorParser <~ skipWs(accept(','))) ~
    (profSidebarFillColorParser <~ skipWs(accept(','))) ~
    (profTextColorParser <~ skipWs(accept(','))) ~
    (profUseBgImageParser <~ skipWs(accept(','))) ~
    (defaultProfileParser <~ skipWs(accept(','))) ~
    (defaultProfileImageParser <~ skipWs(accept(','))) ~
    ((followingParser <~ skipWs(accept(','))) ~>
    (followRequestSentParser <~ skipWs(accept(',')))) <~
    (notificationsParser ~> skipWs(accept('}')))
  ).map {
    /*** SO MUCH BOILERPLATE ***/
    case ((((((((((((((((((((((((((((((((((
        (id, idString),
        name), screenName), location),
        description), url),
        _protected),
        followersCount), friendsCount), listedCount),
        createdAt),
        favouritesCount),
        utcOffset), timeZone), geoEnabled),
        verified), statusesCount),
        lang), contributorsEnabled), isTranslator),
        profileBgColor), profileBgImageUrl), profileBgImageUrlHttps), profileBgTitle),
        profileImageUrl), profileImageUrlHttps), profBannerUrl),
        profileLinkColor), profileSidebarBorderColor),
        profileSidebarFillColor), profileTextColor), profileUseBgImage),
        defaultProfile), defaultProfileImage),
        followingRequestSent) => User(
      id, idString,
      name, screenName, location,
      description, url,
      _protected,
      followersCount, friendsCount, listedCount,
      createdAt,
      favouritesCount,
      utcOffset, timeZone, geoEnabled,
      verified, statusesCount,
      lang, contributorsEnabled, isTranslator,
      profileBgColor, profileBgImageUrl, profileBgImageUrlHttps, profileBgTitle,
      profileImageUrl, profileImageUrlHttps, profBannerUrl,
      profileLinkColor, profileSidebarBorderColor,
      profileSidebarFillColor, profileTextColor, profileUseBgImage,
      defaultProfile, defaultProfileImage,
      followingRequestSent)
  }

  /***********************
   * TweetParser
   ***********************/
  def metadataParser: Parser[Unit] =
    accept("\"metadata\"") ~> skipWs(accept(':')) ~> jsonParser map (_ => ())
  def textParser = accept("\"text\"") ~> skipWs(accept(':')) ~> stringLiteral
  def sourceParser = accept("\"source\"") ~> skipWs(accept(':')) ~> stringLiteral
  def truncatedParser = accept("\"truncated\"") ~> skipWs(accept(':')) ~> bool
  def inReplyToStatusIdParser =
    accept("\"in_reply_to_status_id\"") ~> skipWs(accept(':')) ~> nullOrLong
  def inReplyToStatusIdStringParser =
    accept("\"in_reply_to_status_id_str\"") ~> skipWs(accept(':')) ~> nullOrStrlit
  def inReplyToUserIdParser =
    accept("\"in_reply_to_user_id\"") ~> skipWs(accept(':')) ~> nullOrLong
  def inReplyToUserIdStringParser =
    accept("\"in_reply_to_user_id_str\"") ~> skipWs(accept(':')) ~> nullOrStrlit
  def inReplyToScreenNameParser =
    accept("\"in_reply_to_screen_name\"") ~> skipWs(accept(':')) ~> nullOrStrlit
  def geoParser: Parser[Unit] =
    accept("\"geo\"") ~> skipWs(accept(':')) ~> jsonParser.map(_ => ())
  def coordinatesParser: Parser[Unit] =
    accept("\"coordinates\"") ~> skipWs(accept(':')) ~> jsonParser.map(_ => ())
  def placeParser: Parser[Unit] =
    accept("\"place\"") ~> skipWs(accept(':')) ~> jsonParser.map(_ => ())
  def contributorsParser: Parser[Unit] =
    accept("\"contributors\"") ~> skipWs(accept(':')) ~> accept("null").map(_ => ())
  def retweetedStatusParser: Parser[Unit] =
    accept("\"retweeted_status\"") ~> skipWs(accept(':')) ~> jsonParser.map(_ => ())
  def retweetCountParser = accept("\"retweet_count\"") ~> skipWs(accept(':')) ~> number
  def favoriteCountParser = accept("\"favorite_count\"") ~> skipWs(accept(':')) ~> number
  def favoritedParser = accept("\"favorited\"") ~> skipWs(accept(':')) ~> nullOrBool
  def retweetedParser = accept("\"retweeted\"") ~> skipWs(accept(':')) ~> nullOrBool
  def possiblySensitiveParser = accept("\"possibly_sensitive\"") ~> skipWs(accept(':')) ~> bool

  def tweetParser: Parser[Tweet] = (skipWs(accept('{')) ~>
    (metadataParser <~ skipWs(accept(','))) ~>
    (createdAtParser <~ skipWs(accept(','))) ~
    (idParser <~ skipWs(accept(','))) ~
    (idStrParser <~ skipWs(accept(','))) ~
    (textParser <~ skipWs(accept(','))) ~
    (sourceParser <~ skipWs(accept(','))) ~
    (truncatedParser <~ skipWs(accept(','))) ~
    (inReplyToStatusIdParser <~ skipWs(accept(','))) ~
    (inReplyToStatusIdStringParser <~ skipWs(accept(','))) ~
    (inReplyToUserIdParser <~ skipWs(accept(','))) ~
    (inReplyToUserIdStringParser <~ skipWs(accept(','))) ~
    (inReplyToScreenNameParser <~ skipWs(accept(','))) ~
    (userParser <~ skipWs(accept(','))) ~
    ((geoParser <~ skipWs(accept(','))) ~>
    (coordinatesParser <~ skipWs(accept(','))) ~>
    (placeParser <~ skipWs(accept(','))) ~>
    (contributorsParser <~ skipWs(accept(','))) ~>
    opt(retweetedStatusParser <~ skipWs(accept(','))) ~>
    (retweetCountParser <~ skipWs(accept(',')))) ~
    (favoriteCountParser <~ skipWs(accept(','))) ~
    ((entityParser <~ skipWs(accept(','))) ~>
    (favoritedParser <~ skipWs(accept(',')))) ~
    (retweetedParser <~ skipWs(accept(','))) ~
    opt(possiblySensitiveParser <~ skipWs(accept(','))) ~
    (langParser <~ skipWs(accept('}')))
  ).map {
    case ((((((((((((((((
         (createdAt, id), idString),
         text), source), truncated),
         inReplyToStatusId), inReplyToStatusIdString), inReplyToUserId),
         inReplyToUserIdString), inReplyToScreenName),
         user),
         retweetCount), favoriteCount),
         favorited), retweeted),
         possiblySensitive), lang) => Tweet(
      createdAt, id, idString,
      text, source, truncated,
      inReplyToStatusId, inReplyToStatusIdString, inReplyToUserId,
      inReplyToUserIdString, inReplyToScreenName,
      user,
      retweetCount, favoriteCount,
      favorited, retweeted,
      possiblySensitive, lang)
  }

  def tweetsParser: Parser[List[Tweet]] = (skipWs(accept('[')) ~>
    repsep(tweetParser, skipWs(accept(',')))
    <~ skipWs(accept(']')))


  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "tweets", tweetParser)
  }(range)
}
