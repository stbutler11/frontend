package discussion

import common.{ExecutionContexts, Logging}
import scala.concurrent.Future
import play.api.libs.json.{JsValue, JsArray, JsObject, JsNumber}
import play.api.libs.ws.Response
import discussion.model.{DiscussionKey, Profile, Comment, CommentCount, Switch}
import play.api.mvc.Headers
import discussion.util.Http
import play.api.libs.json.Json

trait DiscussionApi extends Http with ExecutionContexts with Logging {

  protected def GET(url: String, headers: (String, String)*): Future[Response]

  protected val apiRoot: String
  protected val clientHeaderValue: String
  protected val orderBy: String = "newest"
  protected val pageSize: String = "10"

  def commentCounts(ids: String): Future[Seq[CommentCount]] = {
    def onError(response: Response) =
      s"Discussion API: Error loading comment count ids: $ids status: ${response.status} message: ${response.statusText}"
    val apiUrl = s"$apiRoot/getCommentCounts?short-urls=$ids"

    getJsonOrError(apiUrl, onError) map {
      json =>
        json.asInstanceOf[JsObject].fieldSet.toSeq map {
          case (id, JsNumber(i)) => CommentCount(id, i.toInt)
          case bad => throw new RuntimeException(s"never understood $bad")
        }
    }
  }

  private def getCommentPage(key: DiscussionKey, page: String = "1", allResponses: Boolean = false, endpoint: String = ""): Future[CommentPage] = {
    def onError(r: Response) =
      s"Discussion API: Error loading comments id: $key status: ${r.status} message: ${r.statusText}"

    val apiUrl = s"$apiRoot/discussion/$key$endpoint?pageSize=$pageSize&page=$page&orderBy=$orderBy&showSwitches=true" + (if(allResponses) "" else "&maxResponses=3")
    getJsonOrError(apiUrl, onError) map {
      json =>
        val comments = (json \\ "comments")(0).asInstanceOf[JsArray].value map {
          commentJson =>  Comment(commentJson)
        }

        CommentPage(
          id = s"discussion/$key",
          title = (json \ "discussion" \ "title").as[String],
          contentUrl = (json \ "discussion" \ "webUrl").as[String],
          comments = comments,
          commentCount = (json \ "discussion" \ "commentCount").as[Int],
          topLevelCommentCount = (json \ "discussion" \ "topLevelCommentCount").as[Option[Int]].getOrElse(0),
          commenterCount =  (json \ "discussion" \ "commenterCount").as[Option[Int]].getOrElse(0),
          currentPage = (json \ "currentPage").as[Int],
          pages = (json \ "pages").as[Int],
          isClosedForRecommendation = (json \ "discussion" \ "isClosedForRecommendation").as[Boolean],
          switches = (json \ "switches").as[Seq[JsObject]] map { json => Switch(json) }
        )
    }
  }

  def commentsFor(key: DiscussionKey, page: String, allResponses: Boolean = false): Future[CommentPage] = {
    getCommentPage(key, page, allResponses)
  }

  def topCommentsFor(key: DiscussionKey, page: String): Future[CommentPage] = {
    getCommentPage(key, page, endpoint="/topcomments")
  }

  def commentFor(id: Int): Future[Comment] = {
    def onError(r: Response) =
      s"Error loading comment id: $id status: ${r.status} message: ${r.statusText}"

    getJsonOrError(s"$apiRoot/comment/$id?displayResponses=true&displayThreaded=true", onError) map {
      json => 
        val comment = (json \ "comment")
        Comment(comment)
    }
  }

  def commentContext(id: Int): Future[(DiscussionKey, String)] = {
    def onError(r: Response) =
      s"Discussion API: Cannot load comment context, status: ${r.status}, message: ${r.statusText}, response: ${r.body}"

    val apiUrl = s"$apiRoot/comment/$id/context?pageSize=$pageSize&orderBy=$orderBy"

    getJsonOrError(apiUrl, onError) map {
      json =>
        (DiscussionKey((json \ "discussionKey").as[String]), (json \ "page").as[Int].toString)
    }
  }

  def myProfile(headers: Headers): Future[Profile] = {
    def onError(r: Response) =
      s"Discussion API: Error loading profile, status: ${r.status}, message: ${r.statusText}, response: ${r.body}"
    
    val apiUrl = s"$apiRoot/profile/me"
    val authHeader = AuthHeaders.filterHeaders(headers).toSeq

    getJsonOrError(apiUrl, onError, authHeader: _*) map {
      json =>
        Profile(json)
    }
  }

  def postComment(key: DiscussionKey, commentBody: String, auth: String): Future[Comment] = {
    def onError(r: Response) =
      s"Discussion API: Error posting comments, status: ${r.status}, message: ${r.statusText}, response: ${r.body}"

    val apiUrl = s"$apiRoot/discussion/$key/comment"
    val data = Map("body" -> Seq(commentBody), "GU_U" -> Seq(auth))

    postOrError(apiUrl, data, onError) map {
      json =>
        // THIS IS TEST DATA AND NEEDS TO BE POPULATED FORM THE API
        val id = (json \ "message").as[String].toInt
        Comment(Json.parse(s"""{
          "id": $id,
          "body": "$commentBody",
          "responses": [],
          "userProfile": {
            "userId": "",
            "displayName": "",
            "webUrl": "",
            "apiUrl": "",
            "avatar": "",
            "secureAvatarUrl": "",
            "badge": []
          },
          "isoDateTime": "2011-10-10T09:25:49Z",
          "status": "visible",
          "numRecommends": 0,
          "isHighlighted": false
        }"""))
    }
  }

  override protected def getJsonOrError(url: String, onError: (Response) => String, headers: (String, String)*) =
    super.getJsonOrError(url, onError, headers :+ guClientHeader: _*)

  override protected def postOrError(url: String, data: Map[String, Seq[String]], onError: (Response) => String, headers: (String, String)*) =
    super.postOrError(url, data, onError, headers :+ guClientHeader: _*)

  private def guClientHeader = ("GU-Client", clientHeaderValue)
}

object AuthHeaders {
  val guIdToken = "GU-IdentityToken"
  val cookie = "Cookie"
  val all = Set(guIdToken, cookie)

  def filterHeaders(headers: Headers): Map[String, String] = headers.toSimpleMap filterKeys {
    all.contains
  }
}
