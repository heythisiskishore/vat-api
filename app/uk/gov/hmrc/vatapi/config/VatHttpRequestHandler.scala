/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.vatapi.config

import javax.inject.{Inject, Singleton}
import play.api.http.HttpErrorHandler
import play.api.libs.json.Json
import play.api.mvc.Results.BadRequest
import play.api.mvc.{RequestHeader, Result}
import uk.gov.hmrc.api.connector.ServiceLocatorConnector
import uk.gov.hmrc.vatapi.models.{ErrorBadRequest, ErrorCode}

import scala.concurrent.Future

@Singleton
class VatHttpRequestHandler @Inject()(config: AppConfig,
                                           serviceLocatorConnector: ServiceLocatorConnector) extends HttpErrorHandler {

  override def onClientError(request: RequestHeader, statusCode: Int, message: String): Future[Result] = {
    if(statusCode == play.api.http.Status.BAD_REQUEST) {
      message match {
        case "ERROR_VRN_INVALID"       => BadRequest(Json.toJson(ErrorBadRequest(ErrorCode.VRN_INVALID, "The provided Vrn is invalid")))
        case "ERROR_INVALID_DATE"      => BadRequest(Json.toJson(ErrorBadRequest(ErrorCode.INVALID_DATE, "The provided date is invalid")))
        case "ERROR_INVALID_FROM_DATE" => BadRequest(Json.toJson(ErrorBadRequest(ErrorCode.INVALID_FROM_DATE, "The provided from date is invalid")))
        case "ERROR_INVALID_TO_DATE"   => BadRequest(Json.toJson(ErrorBadRequest(ErrorCode.INVALID_TO_DATE, "The provided to date is invalid")))
//        case _                         => result
      }
    }
  }

}
