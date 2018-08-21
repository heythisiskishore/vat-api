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

import javax.inject.Inject
import uk.gov.hmrc.play.config.inject.ServicesConfig

import scala.util.Try
import scala.util.matching.Regex

class RamlFeatureSwitch @Inject()(config: ServicesConfig) {

  def featureSwitchRaml(ramlAsString: String): String = {
    def splitIntoLines(string: String): List[String] = string.split("\\r?\\n").toList
    val ramlLines: List[String] = splitIntoLines(ramlAsString)
    val ramlString: String = ramlLines.mkString("\n")
    updateRaml(ramlString, orderFeaturesFromTags(collectTagsFromRaml(ramlLines)))
  }

  /** Collects all START and STOP tags from the supplied RAML lines */
  private[featureswitch] def collectTagsFromRaml(fileLines: List[String]): List[Tag] = {
    val regex: Regex =
      s"""\\s*#\\s*!([a-zA-Z0-9]+)\\(FEATURE=([a-zA-Z0-9]+)(?:,\\s*FLAG=([a-zA-Z0-9]+))?\\)\\s*""".r

    fileLines.collect {
      case regex(TagType(n), featureName, null) => Tag(n, Feature(featureName, None))
      case regex(TagType(n), featureName, flag) if flag.isBoolean => Tag(n, Feature(featureName, Some(flag.toBoolean)))
      case regex(TagType(n), featureName, flag) => throw new Exception(s"$n tag for feature $featureName has incorrect flag value $flag")
      case regex(tag, featureName, _) => throw new Exception(s"$tag tag for feature $featureName has incorrect tag value, must be either START or STOP")
    }
  }

  /** Tries to fetch the feature flag value from config. Defaults to false if not found or incorrect type */
  private[featureswitch] def getFeatureFlagFromConfig(featureName: String): Boolean = {
    Try(config.getBoolean(s"feature.$featureName")).getOrElse(false)
  }

  /** Sorts a list of tags in the order they should be operated with lowest level nested tags being last */
  private[featureswitch] def orderFeaturesFromTags(tags: List[Tag]): List[Feature] = {
    def order(x: Int, tags: List[Tag], orderedFeatures: List[Feature]): List[Feature] = {
      tags.drop(x) match {
        case Nil => orderedFeatures
        case StartTag(_) :: StartTag(_) :: _ => order(x + 1, tags, orderedFeatures)
        case StartTag(f1) :: StopTag(f2) :: _ if f1 == f2 =>
          val droppedMatchedTags = tags.dropIndex(x).dropIndex(x)
          val next = if (x == 0) x else x - 1
          order(next, droppedMatchedTags, f1 :: orderedFeatures)
        case StartTag(a) :: StopTag(b) :: _ => throw new Exception(s"Start tag $a followed by Stop tag $b")
        case tag :: Nil => throw new Exception(s"Only found ${tag.tagType} tag for feature ${tag.feature.name}${tag.feature.flag.fold("")(flag => s" with flag $flag")}")
        case a :: b :: _ => throw new Exception(s"unknown error with $a followed by $b")
      }
    }
    order(0, tags, List())
  }

  /** Feature switches RAML documentation in the order provided by `features` */
  private[featureswitch] def updateRaml(raml: String, features: List[Feature]): String = {
    def buildFeatureMatcher(featureName: String, flag: Boolean): Regex =
      s"""\\t*#\\s*!(START|STOP)\\(FEATURE=($featureName)(?:,\\s*FLAG=($flag))?\\)\\t*""".r

    def update(x: Int, raml: String, features: List[Feature]): String = {
      features.drop(x) match {
        case Nil => raml
        case feature :: _ =>
          val featureEnabled: Boolean = getFeatureFlagFromConfig(feature.name)
          val featureFlag = feature.flag.fold(true)(identity)
          val featureMatcher = buildFeatureMatcher(feature.name, featureFlag)
          raml.split(featureMatcher.regex, 3).toList match {
            case a :: b :: c :: _ =>
              val updatedRaml: String = {
                if (featureFlag == featureEnabled) a.dropLeadingSpaces + b.trim + c.dropLeadingSpaces
                else a + c.replaceFirst("\n", "").dropLeadingSpaces
              }
              update(0, updatedRaml, features.drop(1))
            case _ => raml
          }
      }
    }
    update(0, raml, features)
  }
}
