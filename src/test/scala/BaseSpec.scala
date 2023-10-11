package example

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import flatspec._
import matchers._

abstract class BaseSpec extends AnyFlatSpec with should.Matchers 
  with OptionValues with Inside with Inspectors with ScalaCheckPropertyChecks
