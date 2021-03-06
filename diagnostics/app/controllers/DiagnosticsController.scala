package controllers

import common._
import play.api.mvc.{ Content => _, _ }
import model.diagnostics.javascript._
import model.diagnostics.viewability._

object DiagnosticsController extends Controller with Logging {

  def js = Action { implicit request =>
    JavaScript.report(request.queryString, request.headers.get("user-agent").getOrElse("UNKNOWN USER AGENT"))
    OnePix()
  } 
  
  def px = Action { implicit request =>
    Alpha.report(request.queryString)
    OnePix()
  } 
  
  def ads(top: Option[Int], bottom: Option[Int], inline: Option[Int], mpu: Option[Int], first: Option[Int], layout: Option[String], variant: Option[String], id: Option[String]) = Action { implicit request =>
    Viewability.report(top, bottom, inline, mpu, first, layout, variant, id)
    OnePix()
  } 

}
