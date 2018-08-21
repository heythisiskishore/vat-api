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

import org.scalatest.{Matchers, WordSpec}
import uk.gov.hmrc.vatapi.mocks.MockServicesConfig

class RamlFeatureSwitchSpec extends WordSpec with Matchers
  with MockServicesConfig {

  class Test {
    val featureSwitch: RamlFeatureSwitch = new RamlFeatureSwitch(
      config = mockServicesConfig
    )
  }

  "collectTags" should {
    "return the tags that match the regex" when {
      "a start tag with a flag set to false is found" in new Test {
        val lines = List("#!START(FEATURE=Test,FLAG=false)")
        val expected = List(Tag(StartTag, Feature("Test", Some(false))))
        featureSwitch.collectTagsFromRaml(lines) shouldBe expected
      }
      "a start tag with a flag set to true is found" in new Test {
        val lines = List("#!START(FEATURE=Test,FLAG=true)")
        val expected = List(Tag(StartTag, Feature("Test", Some(true))))
        featureSwitch.collectTagsFromRaml(lines) shouldBe expected
      }
      "a start tag with no flag is found" in new Test {
        val lines = List("#!START(FEATURE=Test)")
        val expected = List(Tag(StartTag, Feature("Test", None)))
        featureSwitch.collectTagsFromRaml(lines) shouldBe expected
      }
      "a stop tag with a flag set to false is found" in new Test {
        val lines = List("#!STOP(FEATURE=Test,FLAG=false)")
        val expected = List(Tag(StopTag, Feature("Test", Some(false))))
        featureSwitch.collectTagsFromRaml(lines) shouldBe expected
      }
      "a stop tag with a flag set to true is found" in new Test {
        val lines = List("#!STOP(FEATURE=Test,FLAG=true)")
        val expected = List(Tag(StopTag, Feature("Test", Some(true))))
        featureSwitch.collectTagsFromRaml(lines) shouldBe expected
      }
      "a stop tag with no flag is found" in new Test {
        val lines = List("#!STOP(FEATURE=Test)")
        val expected = List(Tag(StopTag, Feature("Test", None)))
        featureSwitch.collectTagsFromRaml(lines) shouldBe expected
      }
      "a start and stop tag are found with content" in new Test {
        val lines = List(
          "some content",
          "#!START(FEATURE=Test,FLAG=true)",
          "some content",
          "some content",
          "#!STOP(FEATURE=Test,FLAG=true)",
          "some content"
        )
        val expected = List(
          Tag(StartTag, Feature("Test", Some(true))),
          Tag(StopTag, Feature("Test", Some(true)))
        )
        featureSwitch.collectTagsFromRaml(lines) shouldBe expected
      }
    }

    "throw an exception" when {
      "a start tag with a non-boolean flag is found" in new Test {
        val lines = List("#!START(FEATURE=Test,FLAG=notBool)")
        val ex: Exception = the [Exception] thrownBy featureSwitch.collectTagsFromRaml(lines)
        ex.getMessage shouldBe "START tag for feature Test has incorrect flag value notBool"
      }
      "a tag has the wrong tag name" in new Test {
        val lines = List("#!WRONG(FEATURE=Test,FLAG=true)")
        val ex: Exception = the [Exception] thrownBy featureSwitch.collectTagsFromRaml(lines)
        ex.getMessage shouldBe "WRONG tag for feature Test has incorrect tag value, must be either START or STOP"
      }
      "a tag has the wrong tag name and the wrong flag name" in new Test {
        val lines = List("#!WRONG(FEATURE=Test,FLAG=notBool)")
        val ex: Exception = the [Exception] thrownBy featureSwitch.collectTagsFromRaml(lines)
        ex.getMessage shouldBe "WRONG tag for feature Test has incorrect tag value, must be either START or STOP"
      }
    }
  }

  "getFeatureFlagFromConfig" should {
    "return true" when {
      "the flag from config is true" in new Test {
        MockedServicesConfig.getBoolean("feature.test").returns(true)
        featureSwitch.getFeatureFlagFromConfig("test") shouldBe true
      }
    }

    "return false" when {
      "the flag from config is false" in new Test {
        MockedServicesConfig.getBoolean("feature.test").returns(false)
        featureSwitch.getFeatureFlagFromConfig("test") shouldBe false
      }
      "config can't find the feature flag and throws an exception" in new Test {
        MockedServicesConfig.getBoolean("feature.test").throws(new RuntimeException())
        featureSwitch.getFeatureFlagFromConfig("test") shouldBe false
      }
    }
  }

  "orderFeatures" should {
    "return an empty list of features" when {
      "an empty list of tags is supplied" in new Test {
        val testNodes: List[Tag] = List()
        featureSwitch.orderFeaturesFromTags(testNodes) shouldBe List()
      }
    }

    "return a list of features in the order they should be switched" when {
      "A single set of tags is supplied" in new Test {
        val testNodes: List[Tag] = List(
          Tag(StartTag, Feature("Test1", None)),
          Tag(StopTag, Feature("Test1", None))
        )
        featureSwitch.orderFeaturesFromTags(testNodes) shouldBe List(Feature("Test1", None))
      }
      "two sets of tags are supplied sequentially" in new Test {
        val testNodes: List[Tag] = List(
          Tag(StartTag, Feature("Test1", None)),
          Tag(StopTag, Feature("Test1", None)),
          Tag(StartTag, Feature("Test2", None)),
          Tag(StopTag, Feature("Test2", None))
        )
        featureSwitch.orderFeaturesFromTags(testNodes) shouldBe
          List(Feature("Test2", None), Feature("Test1", None))
      }
      "two sets of the same tags are supplied sequentially" in new Test {
        val testNodes: List[Tag] = List(
          Tag(StartTag, Feature("Test1", None)),
          Tag(StopTag, Feature("Test1", None)),
          Tag(StartTag, Feature("Test1", None)),
          Tag(StopTag, Feature("Test1", None))
        )
        featureSwitch.orderFeaturesFromTags(testNodes) shouldBe
          List(Feature("Test1", None), Feature("Test1", None))
      }
      "two feature switches for the same feature are supplied" in new Test {
        val testNodes: List[Tag] = List(
          Tag(StartTag, Feature("Test1", Some(true))),
          Tag(StopTag, Feature("Test1", Some(true))),
          Tag(StartTag, Feature("Test1", Some(false))),
          Tag(StopTag, Feature("Test1", Some(false))),
          Tag(StartTag, Feature("Test1", Some(true))),
          Tag(StopTag, Feature("Test1", Some(true))),
          Tag(StartTag, Feature("Test1", Some(false))),
          Tag(StopTag, Feature("Test1", Some(false)))
        )
        featureSwitch.orderFeaturesFromTags(testNodes) shouldBe
          List(
            Feature("Test1", Some(false)),
            Feature("Test1", Some(true)),
            Feature("Test1", Some(false)),
            Feature("Test1", Some(true))
          )
      }
      "two sets of tags are supplied nested" in new Test {
        val testNodes: List[Tag] = List(
          Tag(StartTag, Feature("Test1", None)),
            Tag(StartTag, Feature("Test2", None)),
            Tag(StopTag, Feature("Test2", None)),
          Tag(StopTag, Feature("Test1", None))
        )
        featureSwitch.orderFeaturesFromTags(testNodes) shouldBe
          List(Feature("Test1", None), Feature("Test2", None))
      }
      "three sets of tags are supplied nested" in new Test {
        val testNodes: List[Tag] = List(
          Tag(StartTag, Feature("Test1", None)),
            Tag(StartTag, Feature("Test2", None)),
              Tag(StartTag, Feature("Test3", None)),
              Tag(StopTag, Feature("Test3", None)),
            Tag(StopTag, Feature("Test2", None)),
          Tag(StopTag, Feature("Test1", None))
        )
        featureSwitch.orderFeaturesFromTags(testNodes) shouldBe
          List(Feature("Test1", None), Feature("Test2", None), Feature("Test3", None))
      }
      "three sets of tags are supplied, two are nested and one is sequential" in new Test {
        val testNodes: List[Tag] = List(
          Tag(StartTag, Feature("Test1", None)),
            Tag(StartTag, Feature("Test2", None)),
            Tag(StopTag, Feature("Test2", None)),
          Tag(StopTag, Feature("Test1", None)),
          Tag(StartTag, Feature("Test3", None)),
          Tag(StopTag, Feature("Test3", None))
        )
        featureSwitch.orderFeaturesFromTags(testNodes) shouldBe
          List(Feature("Test3", None), Feature("Test1", None), Feature("Test2", None))
      }
    }

    "throw an exception" when {
      "a list of a single Start Tag is supplied for a feature" in new Test {
        val testNodes: List[Tag] = List(Tag(StartTag, Feature("Test1", None)))
        val ex: Exception = the [Exception] thrownBy featureSwitch.orderFeaturesFromTags(testNodes)
        ex.getMessage shouldBe "Only found START tag for feature Test1"
      }

      "a list of a single Start Tag is supplied for a feature among other features" in new Test {
        val testNodes: List[Tag] = List(
          Tag(StartTag, Feature("Test1", None)),
          Tag(StopTag, Feature("Test1", None)),
          Tag(StartTag, Feature("Test2", None))
        )
        val ex: Exception = the [Exception] thrownBy featureSwitch.orderFeaturesFromTags(testNodes)
        ex.getMessage shouldBe "Only found START tag for feature Test2"
      }
    }
  }

  "updateRaml" should {
    "return a raml string that contains only the fields that are meant to be displayed" when {
      "a feature switch exists and the flag from config is true" in new Test{
        val testRaml: String =
          """#!START(FEATURE=Test1,FLAG=true)
            |testkey: true
            |#!STOP(FEATURE=Test1,FLAG=true)
            |#!START(FEATURE=Test1,FLAG=false)
            |testkey: false
            |#!STOP(FEATURE=Test1,FLAG=false)""".stripMargin
        val expected: String =
          "testkey: true\n"
        val testFeatures = List(Feature("Test1", Some(true)), Feature("Test1", Some(false)))

        MockedServicesConfig.getBoolean("feature.Test1").returns(true)
        featureSwitch.updateRaml(testRaml, testFeatures) shouldBe expected
      }
      "a feature switch exists and the flag from config is false" in new Test{
        val testRaml: String =
          """
            |#!START(FEATURE=Test1,FLAG=true)
            |testkey: true
            |#!STOP(FEATURE=Test1,FLAG=true)
            |#!START(FEATURE=Test1,FLAG=false)
            |testkey: false
            |#!STOP(FEATURE=Test1,FLAG=false)
          """.stripMargin
        val expected: String =
          """
            |testkey: false
          """.stripMargin
        val testFeatures = List(Feature("Test1", Some(true)), Feature("Test1", Some(false)))

        MockedServicesConfig.getBoolean("feature.Test1").returns(false)
        featureSwitch.updateRaml(testRaml, testFeatures) shouldBe expected
      }
      "a feature switch exists without a flag and the flag from config is true" in new Test{
        val testRaml: String =
          """#!START(FEATURE=Test1)
            |testkey: true
            |#!STOP(FEATURE=Test1)""".stripMargin
        val expected: String = "testkey: true"
        val testFeatures = List(Feature("Test1", None))

        MockedServicesConfig.getBoolean("feature.Test1").returns(true)
        featureSwitch.updateRaml(testRaml, testFeatures) shouldBe expected
      }
      "a feature switch exists without a flag and the flag from config is false" in new Test{
        val testRaml: String =
          """#!START(FEATURE=Test1)
            |testkey: true
            |#!STOP(FEATURE=Test1)""".stripMargin
        val expected: String = ""
        val testFeatures = List(Feature("Test1", None))

        MockedServicesConfig.getBoolean("feature.Test1").returns(false)
        featureSwitch.updateRaml(testRaml, testFeatures) shouldBe expected
      }
      "a feature switch exists without a flag with content before and after the feature switch and the flag from config is true" in new Test{
        val testRaml: String =
          """
            |some: field
            |#!START(FEATURE=Test1)
            |testkey: true
            |#!STOP(FEATURE=Test1)
            |some: other field
          """.stripMargin
        val expected: String =
          """
            |some: field
            |testkey: true
            |some: other field
          """.stripMargin
        val testFeatures = List(Feature("Test1", None))

        MockedServicesConfig.getBoolean("feature.Test1").returns(true)
        featureSwitch.updateRaml(testRaml, testFeatures) shouldBe expected
      }
      "a feature switch exists without a flag with content before the feature switch and the flag from config is true" in new Test{
        val testRaml: String =
          """
            |some: field
            |#!START(FEATURE=Test1)
            |testkey: true
            |#!STOP(FEATURE=Test1)
          """.stripMargin
        val expected: String =
          """
            |some: field
            |testkey: true
          """.stripMargin
        val testFeatures = List(Feature("Test1", None))

        MockedServicesConfig.getBoolean("feature.Test1").returns(true)
        featureSwitch.updateRaml(testRaml, testFeatures) shouldBe expected
      }
      "a feature switch exists without a flag with content after the feature switch and the flag from config is true" in new Test{
        val testRaml: String =
          """
            |#!START(FEATURE=Test1)
            |testkey: true
            |#!STOP(FEATURE=Test1)
            |some: field
          """.stripMargin
        val expected: String =
          """
            |testkey: true
            |some: field
          """.stripMargin
        val testFeatures = List(Feature("Test1", None))

        MockedServicesConfig.getBoolean("feature.Test1").returns(true)
        featureSwitch.updateRaml(testRaml, testFeatures) shouldBe expected
      }
      "two of the same feature switch exists and the flag from config is true" in new Test {
        val testRaml: String =
          """some: field1
            |
            |#!START(FEATURE=Test1,FLAG=true)
            |testkey1: true
            |#!STOP(FEATURE=Test1,FLAG=true)
            |#!START(FEATURE=Test1,FLAG=false)
            |testkey1: false
            |#!STOP(FEATURE=Test1,FLAG=false)
            |
            |some: field2
            |
            |#!START(FEATURE=Test1,FLAG=true)
            |testkey2: true
            |#!STOP(FEATURE=Test1,FLAG=true)
            |#!START(FEATURE=Test1,FLAG=false)
            |testkey2: false
            |#!STOP(FEATURE=Test1,FLAG=false)
            |
            |some: field3""".stripMargin

        val expected: String =
          """some: field1
            |
            |testkey1: true
            |
            |some: field2
            |
            |testkey2: true
            |
            |some: field3""".stripMargin

        val testFeatures = List(
          Feature("Test1", Some(false)),
          Feature("Test1", Some(true)),
          Feature("Test1", Some(false)),
          Feature("Test1", Some(true))
        )

        MockedServicesConfig.getBoolean("feature.Test1").returns(true)
        featureSwitch.updateRaml(testRaml, testFeatures) shouldBe expected
      }
      "two of the same feature switch exists with a nested feature flag and the flag from config for Test1 is true" in new Test {
        val testRaml: String =
          """some: field1
            |
            |#!START(FEATURE=Test1,FLAG=true)
            |testkey1: true
            |#!STOP(FEATURE=Test1,FLAG=true)
            |#!START(FEATURE=Test1,FLAG=false)
            |testkey1: false
            |#!START(FEATURE=Test2)
            |testkey2: true
            |#!START(FEATURE=Test2)
            |#!STOP(FEATURE=Test1,FLAG=false)
            |
            |some: field2""".stripMargin

        val expected: String =
          """some: field1
            |
            |testkey1: true
            |
            |some: field2""".stripMargin

        val testFeatures = List(
          Feature("Test1", Some(false)),
          Feature("Test1", Some(true)),
          Feature("Test2", None)
        )

        MockedServicesConfig.getBoolean("feature.Test1").returns(true)
        featureSwitch.updateRaml(testRaml, testFeatures) shouldBe expected
      }
    }

    "featureSwitchRaml" should {
      "return a correctly updated raml string" when {
        "the flag from config for feature 'Test1' is set to true" in new Test {
          val testRaml: String =
            """some: field1
              |
              |#!START(FEATURE=Test1,FLAG=true)
              |testkey1: true
              |#!STOP(FEATURE=Test1,FLAG=true)
              |#!START(FEATURE=Test1,FLAG=false)
              |testkey1: false
              |#!STOP(FEATURE=Test1,FLAG=false)
              |
              |some: field2
              |
              |#!START(FEATURE=Test1,FLAG=true)
              |testkey2: true
              |#!STOP(FEATURE=Test1,FLAG=true)
              |#!START(FEATURE=Test1,FLAG=false)
              |testkey2: false
              |#!STOP(FEATURE=Test1,FLAG=false)
              |
              |some: field3""".stripMargin

          val expected: String =
            """some: field1
              |
              |testkey1: true
              |
              |some: field2
              |
              |testkey2: true
              |
              |some: field3""".stripMargin

          MockedServicesConfig.getBoolean("feature.Test1").returns(true)
          featureSwitch.featureSwitchRaml(testRaml) shouldBe expected
        }
        "a real world example is supplied" in new Test {
          val testRaml: String =
              """
                |traits:
                |  contentType:
                |    headers:
                |      Content-Type:
                |        #!START(FEATURE=test,FLAG=true)
                |        description: Specifies the format of the request body, which must be JSON.
                |        #!STOP(FEATURE=test,FLAG=true)
                |        #!START(FEATURE=test,FLAG=false)
                |        description: false
                |        #!STOP(FEATURE=test,FLAG=false)
                |        type: string
                |        required: true
                |        example: application/json
              """.stripMargin

          val expected: String =
              """
                |traits:
                |  contentType:
                |    headers:
                |      Content-Type:
                |        description: Specifies the format of the request body, which must be JSON.
                |        type: string
                |        required: true
                |        example: application/json
              """.stripMargin

          MockedServicesConfig.getBoolean("feature.test").returns(true)
          println(featureSwitch.featureSwitchRaml(testRaml))
          println(expected)
          featureSwitch.featureSwitchRaml(testRaml).trim shouldBe expected.trim
        }
      }
    }
  }
}
