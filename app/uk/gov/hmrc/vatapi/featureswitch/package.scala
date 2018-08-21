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

package uk.gov.hmrc.vatapi

import scala.util.Try

package object featureswitch {
  implicit class ListOps[T](xs: List[T]) {
    def dropIndex(n: Int): List[T] = {
      val (l1, l2) = xs splitAt n
      l1 ::: (l2 drop 1)
    }
  }

  implicit class StringOps(s: String){
    def isBoolean: Boolean = Try(s.toLowerCase.toBoolean).isSuccess
    def dropLeadingSpaces: String = s.dropWhile(_ == ' ')
  }
}
