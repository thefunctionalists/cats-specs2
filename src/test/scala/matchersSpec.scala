package org.specs2.cats

import cats.data.Xor
import cats.data.Validated

class CatsMatchersSpec extends org.specs2.Specification with CatsMatchers {
  def is = s2"""
   Xor matchers ${XorMatchers}
   Validated matchers ${ValidatedMatchers}
  """

  def XorMatchers = s2"""
      Xor.Right ${XorMatcherSpec().right}
      Xor.Left  ${XorMatcherSpec().left}
  """

  def ValidatedMatchers = s2"""
      Validated.Valid ${ValidatedMatcherSpec().valid}
      Validated.InValid ${ValidatedMatcherSpec().invalid}
  """

  case class XorMatcherSpec() {
    def right = (Xor.Right(7) must beXorRight) and
      (Xor.Right(7) must beXorRight(7)) and
      (Xor.Right(7) must be xorRight) and
      (Xor.Right(7) must beXorRight.like {
        case v => v === 7
      })

    def left = (Xor.Left(7) must beXorLeft) and
      (Xor.Left(7) must beXorLeft(7)) and
      (Xor.Left(7) must be beXorLeft) and
      (Xor.Left(7) must beXorLeft.like {
        case v => v !== 8
      })

  }

  case class ValidatedMatcherSpec() {
    def valid = (Validated.Valid(7) must beValid) and
      (Validated.Valid(7) must beValid(7)) and
      (Validated.Valid(7) must be valid) and
      (Validated.Valid(7) must beValid.like {
        case v => v === 7
      })

    def invalid = (Validated.Invalid(7) must beInvalid) and
      (Validated.Invalid(7) must beInvalid(7)) and
      (Validated.Invalid(7) must be invalid) and
      (Validated.Invalid(7) must beInvalid.like {
        case v => v !== 8
      })

  }

}

