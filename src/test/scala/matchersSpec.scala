package org.specs2.cats

import cats.data.Xor

class CatsMatchersSpec extends org.specs2.Specification with CatsMatchers {
  def is = s2"""
   Xor matchers ${XorMatchers}
  """

  def XorMatchers = s2"""
      Xor.Right ${XorMatcherSpec().right}
      Xor.Left  ${XorMatcherSpec().left}
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

}

