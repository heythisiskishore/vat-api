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

package uk.gov.hmrc.vatapi.featureswitch

import scala.util.Try

case class Tag(tagType: TagType, feature: Feature)

object Tag {
  val START = "START"
  val STOP = "STOP"
}

sealed trait TagType{
  def toString: String

  def unapply(tag: Tag): Option[Feature] =
    if(tag.tagType == this) Some(tag.feature) else None
}

object StartTag extends TagType {
  override def toString: String = Tag.START
}

object StopTag extends TagType{
  override def toString: String = Tag.STOP
}

object TagType {
  def apply(s: String): TagType = s match {
    case Tag.START => StartTag
    case Tag.STOP => StopTag
    case _ => throw new Exception(s + " is not a valid tag type")
  }

  def unapply(arg: String): Option[TagType] = Try(this(arg)).toOption
}
