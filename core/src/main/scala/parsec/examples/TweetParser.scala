package parsec.examples

import parsec._

object TweetParser extends JSONParsers {

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

  def main(args: Array[String]): Unit = {
    println("greetings lion, this one a tweet parser")

    import scala.io.Source
    val fileName = "data/tweets.json"
    val fileContent = Source.fromFile(fileName).mkString

    //scalastyle:off line.size.limit
    val tweetSource = """{

      "metadata": {
        "result_type": "recent",
        "iso_language_code": "fr"
      },
      "created_at": "Wed Sep 04 13:00:23 +0000 2013",
      "id": 3.7524193330596e+17,
      "id_str": "375241933305958400",
      "text": "2 426 319 \u20ac\nManque \u00e0 gagner de R. Federer dans les tournois du grand chelem entre 2013 et 2012\nInfos @lequipe du jour",
      "source": "<a href=\"http:\/\/twitter.com\/download\/iphone\" rel=\"nofollow\">Twitter for iPhone<\/a>",
      "truncated": false,
      "in_reply_to_status_id": null,
      "in_reply_to_status_id_str": null,
      "in_reply_to_user_id": null,
      "in_reply_to_user_id_str": null,
      "in_reply_to_screen_name": null,
      "user": {
        "id": 150792620,
        "id_str": "150792620",
        "name": "eSchnub",
        "screen_name": "eschnub",
        "location": "Francia",
        "description": "",
        "url": null,
        "entities": {
          "description": {
            "urls": [

            ]
          }
        },
        "protected": false,
        "followers_count": 63,
        "friends_count": 153,
        "listed_count": 0,
        "created_at": "Tue Jun 01 21:02:57 +0000 2010",
        "favourites_count": 1,
        "utc_offset": null,
        "time_zone": null,
        "geo_enabled": true,
        "verified": false,
        "statuses_count": 967,
        "lang": "fr",
        "contributors_enabled": false,
        "is_translator": false,
        "profile_background_color": "C0DEED",
        "profile_background_image_url": "http:\/\/a0.twimg.com\/images\/themes\/theme1\/bg.png",
        "profile_background_image_url_https": "https:\/\/si0.twimg.com\/images\/themes\/theme1\/bg.png",
        "profile_background_tile": false,
        "profile_image_url": "http:\/\/a0.twimg.com\/profile_images\/3120357149\/a38d532057f488d0ab505af89e4f5f97_normal.jpeg",
        "profile_image_url_https": "https:\/\/si0.twimg.com\/profile_images\/3120357149\/a38d532057f488d0ab505af89e4f5f97_normal.jpeg",
        "profile_link_color": "0084B4",
        "profile_sidebar_border_color": "C0DEED",
        "profile_sidebar_fill_color": "DDEEF6",
        "profile_text_color": "333333",
        "profile_use_background_image": true,
        "default_profile": true,
        "default_profile_image": false,
        "following": false,
        "follow_request_sent": false,
        "notifications": false
      },
      "geo": {
        "type": "Point",
        "coordinates": [
          48.8274078,
          2.24161782
        ]
      },
      "coordinates": {
        "type": "Point",
        "coordinates": [
          2.24161782,
          48.8274078
        ]
      },
      "place": {
        "id": "cc247ade27d3b837",
        "url": "https:\/\/api.twitter.com\/1.1\/geo\/id\/cc247ade27d3b837.json",
        "place_type": "city",
        "name": "Boulogne-Billancourt",
        "full_name": "Boulogne-Billancourt, Hauts-de-Seine",
        "country_code": "FR",
        "country": "France",
        "bounding_box": {
          "type": "Polygon",
          "coordinates": [
            [
              [
                2.2228807,
                48.8214846
              ],
              [
                2.262982,
                48.8214846
              ],
              [
                2.262982,
                48.8534246
              ],
              [
                2.2228807,
                48.8534246
              ]
            ]
          ]
        },
        "attributes": {

        }
      },
      "contributors": null,
      "retweet_count": 0,
      "favorite_count": 0,
      "entities": {
        "hashtags": [

        ],
        "symbols": [

        ],
        "urls": [

        ],
        "user_mentions": [
          {
            "screen_name": "lequipe",
            "name": "L'\u00c9QUIPE",
            "id": 18936161,
            "id_str": "18936161",
            "indices": [
              101,
              109
            ]
          }
        ]
      },
      "favorited": false,
      "retweeted": false,
      "lang": "fr"
    }"""
    //scalastyle:on line.size.limit

    println(tweetSource.subSequence(2400, 2900))
    val myReader = CharReader(fileContent.toArray)
    //val myReader = CharReader(tweetSource.toArray)
    val Success(res, rest) = tweetsParser(myReader)
    res match {
      case ls => println(ls)
      case _ => println("something else")
    }
  }

}
