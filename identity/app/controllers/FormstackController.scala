package controllers

import play.api.mvc._
import model.IdentityPage
import common.ExecutionContexts
import services.{IdentityUrlBuilder, IdRequestParser, ReturnUrlVerifier}
import com.google.inject.{Inject, Singleton}
import utils.SafeLogging
import scala.concurrent.Future
import formstack.{FormstackApi, FormstackForm}
import conf.Switches


@Singleton
class FormstackController @Inject()(returnUrlVerifier: ReturnUrlVerifier,
                                    idRequestParser: IdRequestParser,
                                    idUrlBuilder: IdentityUrlBuilder,
                                    authAction: utils.AuthAction,
                                    formStackApi: FormstackApi)
  extends Controller with ExecutionContexts with SafeLogging {

  val page = IdentityPage("/form", "Form", "formstack")

  def formstackForm(formReference: String) = authAction.async { implicit request =>
    if (Switches.IdentityEthicalAwardsSwitch.isSwitchedOn) {
      FormstackForm.extractFromSlug(formReference).map { formstackForm =>
        formStackApi.checkForm(formstackForm).map {
          case Right(_) => {
            logger.trace(s"Rendering formstack form ${formstackForm.formId}")
            Ok(views.html.formstack.formstackForm(page, formstackForm))
          }
          case Left(errors) => {
            logger.warn(s"Unable to render formstack form ${formstackForm.formReference}, $errors")
            new Status(errors.map(_.statusCode).max)(views.html.formstack.formstackFormNotFound(page))
          }
        }
      }.getOrElse {
        Future.successful(NotFound(views.html.formstack.formstackFormNotFound(page)))
      }
    } else {
      Future.successful(NotFound(views.html.errors._404()))
    }
  }

  def complete = Action { implicit request =>
    if (Switches.IdentityEthicalAwardsSwitch.isSwitchedOn) {
      Ok(views.html.formstack.formstackComplete(page))
    } else {
      NotFound(views.html.errors._404())
    }
  }
}