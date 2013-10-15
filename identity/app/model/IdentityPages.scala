package model

import tracking.Omniture

class IdentityPage(id: String, webTitle: String, analyticsName: String, extraMetadata: Option[Map[String, Any]] = None, overriddenMetadata: Option[Map[String, Any]] = None)
  extends Page(id, "identity", webTitle, analyticsName) {

  override def metaData: Map[String, Any] = overriddenMetadata.getOrElse(super.metaData + ("blockAds" -> true) ++ extraMetadata.getOrElse(Map.empty))
}
object IdentityPage {
  def apply(id: String, webTitle: String, analyticsName: String): IdentityPage with Omniture = {
    new IdentityPage(id, webTitle, analyticsName, None) with Omniture
  }
}

object SlimIdentityPage {
  def apply(id: String, webTitle: String, analyticsName: String): IdentityPage with Omniture = {
    new IdentityPage(id, webTitle, analyticsName, extraMetadata = Some(Map("showAdBlock" -> false, "showMainNav" -> false))) with Omniture
  }
}
