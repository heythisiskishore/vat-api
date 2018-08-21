/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.vatapi.controllers.definition

import javax.inject.Inject
import play.api.Application
import play.api.http.LazyHttpErrorHandler
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.api.controllers.DocumentationController
import uk.gov.hmrc.vatapi.controllers.definition.JsonFormatters._
import uk.gov.hmrc.vatapi.featureswitch.RamlFeatureSwitch

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class VatDocumentationController @Inject()(app: Application,
                                           featureSwitch: RamlFeatureSwitch) extends DocumentationController(LazyHttpErrorHandler) {

  override def definition() = Action {
    Ok(Json.toJson(VatApiDefinition.definition))
  }

  override def conf(version: String, file: String): Action[AnyContent] = Action.async{
    implicit req =>
      super.at(s"/public/api/conf/$version", file)(req).flatMap{ res =>
        if(file.endsWith(".raml")) {
          val raml = extractResultBody(res)
          val updatedRaml = raml.map(featureSwitch.featureSwitchRaml)
          updatedRaml.map(body => new Status(res.header.status)(body))
        } else Future.successful(res)
      }
  }

  private def extractResultBody(res: Result): Future[String] = {
    val Fbytes: Future[List[Byte]] = res.body.consumeData(app.materializer).map(_.toList)
    Fbytes.map(bytes => new String(bytes.toArray))
  }
}
