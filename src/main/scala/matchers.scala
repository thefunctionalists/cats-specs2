package org.specs2.cats

import org.specs2.matcher._
import org.specs2.text.Quote._
import org.specs2.execute.{ Failure, Result }

trait CatsMatchers
  extends XorBaseMatchers
  with XorBeHaveMatchers
  with ValidatedBaseMatchers
  with ValidatedBeHaveMatchers

private[specs2] trait XorBaseMatchers {
  import cats.data.Xor

  def beXorRight[T](t: => T) =
    new Matcher[Xor[_, T]] {
      def apply[S <: Xor[_, T]](value: Expectable[S]) = {
        val expected = t
        result(
          value.value == Xor.Right(t),
          s"${value.description} is Right[T] with value ${q(expected)}",
          s"${value.description} is not Right[T] with value ${q(expected)}",
          value
        )
      }
    }

  def beXorRight[T] =
    new Matcher[Xor[_, T]] {
      def apply[S <: Xor[_, T]](value: Expectable[S]) = {
        result(
          value.value.isRight,
          s"${value.description} is Right[T]",
          s"${value.description} is not Right[T]",
          value
        )
      }

      def like(f: PartialFunction[T, MatchResult[_]]) = this and partialMatcher(f)

      private def partialMatcher(f: PartialFunction[T, MatchResult[_]]) = new Matcher[Xor[_, T]] {
        def apply[S <: Xor[_, T]](value: Expectable[S]) = {
          val res: Result = value.value match {
            case Xor.Right(t) if f.isDefinedAt(t) => f(t).toResult
            case Xor.Right(t) if !f.isDefinedAt(t) => Failure("function undedined")
            case _ => Failure("no match")
          }
          result(
            res.isSuccess,
            s"${value.description} is Right[T] and ${res.message}",
            s"${value.description} is Right[T] but ${res.message}",
            value
          )
        }
      }
    }

  def beXorLeft[T](t: => T) =
    new Matcher[Xor[T, _]] {
      def apply[S <: Xor[T, _]](value: Expectable[S]) = {
        val expected = t
        result(
          value.value == Xor.Left(t),
          s"${value.description} is Left[T] with value ${q(expected)}",
          s"${value.description} is not Left[T] with value ${q(expected)}",
          value
        )
      }
    }

  def beXorLeft[T] = new Matcher[Xor[T, _]] {
    def apply[S <: Xor[T, _]](value: Expectable[S]) = {
      result(
        value.value.isLeft,
        s"${value.description} is Left[T]",
        s"${value.description} is not Left[T]",
        value
      )
    }

    def like(f: PartialFunction[T, MatchResult[_]]) = this and partialMatcher(f)

    private def partialMatcher(f: PartialFunction[T, MatchResult[_]]) = new Matcher[Xor[T, _]] {
      def apply[S <: Xor[T, _]](value: Expectable[S]) = {
        val res: Result = value.value match {
          case Xor.Left(t) if f.isDefinedAt(t) => f(t).toResult
          case Xor.Left(t) if !f.isDefinedAt(t) => Failure("function undefined")
          case other => Failure("no match")
        }
        result(
          res.isSuccess,
          s"${value.description} is Left[T] and ${res.message}",
          s"${value.description} is Left[T] but  ${res.message}",
          value
        )
      }
    }
  }
}

private[specs2] trait XorBeHaveMatchers { outer: XorBaseMatchers =>
  import cats.data.Xor

  implicit def toXorResultMatcher[F, S](result: MatchResult[Xor[F, S]]) =
    new XorResultMatcher(result)

  class XorResultMatcher[F, S](result: MatchResult[Xor[F, S]]) {
    def xorLeft(f: => F) = result(outer beXorLeft f)
    def beXorLeft(f: => F) = result(outer beXorLeft f)
    def xorRight(s: => S) = result(outer beXorRight s)
    def beXorRight(s: => S) = result(outer beXorRight s)

    def xorLeft = result(outer.beXorLeft)
    def beXorLeft = result(outer.beXorLeft)
    def xorRight = result(outer.beXorRight)
    def beXorRight = result(outer.beXorRight)
  }
}

private[specs2] trait ValidatedBaseMatchers {
  import cats.data.Validated
  def beValid[T](t: => T) =
    new Matcher[Validated[_, T]] {
      def apply[S <: Validated[_, T]](value: Expectable[S]) = {
        val expected = t
        result(
          value.value == Validated.Valid(t),
          s"${value.description} is Valid[T] with value ${q(expected)}",
          s"${value.description} is not Valid[T] with value ${q(expected)}",
          value
        )
      }
    }

  def beValid[T] = new Matcher[Validated[_, T]] {
    def apply[S <: Validated[_, T]](value: Expectable[S]) = {
      result(
        value.value.isValid,
        s"${value.description} is Valid[T]",
        s"${value.description} is not Valid[T]",
        value
      )
    }

    def like(f: PartialFunction[T, MatchResult[_]]) = this and partialMatcher(f)

    private def partialMatcher(f: PartialFunction[T, MatchResult[_]]) = new Matcher[Validated[_, T]] {
      def apply[S <: Validated[_, T]](value: Expectable[S]) = {
        val res: Result = value.value match {
          case Validated.Valid(t) if f.isDefinedAt(t) => f(t).toResult
          case Validated.Valid(t) if !f.isDefinedAt(t) => Failure("function undefined")
          case other => Failure("no match")
        }
        result(
          res.isSuccess,
          value.description + " is Valid[T] and " + res.message,
          value.description + " is Valid[T] but " + res.message,
          value
        )
      }
    }

  }

  def beInvalid[T](t: => T) =
    new Matcher[Validated[T, _]] {
      def apply[S <: Validated[T, _]](value: Expectable[S]) = {
        val expected = t
        result(
          value.value == Validated.Invalid(t),
          s"${value.description} is Invalid[T] with value ${q(expected)}",
          s"${value.description} is not Invalid[T] with value ${q(expected)}",
          value
        )
      }
    }

  def beInvalid[T] = new Matcher[Validated[T, _]] {
    def apply[S <: Validated[T, _]](value: Expectable[S]) = {
      result(
        value.value.isInvalid,
        s"${value.description} is Invalid[T]",
        s"${value.description} is not Invalid[T]",
        value
      )
    }

    def like(f: PartialFunction[T, MatchResult[_]]) = this and partialMatcher(f)

    private def partialMatcher(f: PartialFunction[T, MatchResult[_]]) = new Matcher[Validated[T, _]] {
      def apply[S <: Validated[T, _]](value: Expectable[S]) = {
        val res: Result = value.value match {
          case Validated.Invalid(t) if f.isDefinedAt(t) => f(t).toResult
          case Validated.Invalid(t) if !f.isDefinedAt(t) => Failure("function undefined")
          case other => Failure("no match")
        }
        result(
          res.isSuccess,
          value.description + " is Invalid[T] and " + res.message,
          value.description + " is Invalid[T] but " + res.message,
          value
        )
      }
    }
  }

}

private[specs2] trait ValidatedBeHaveMatchers { outer: ValidatedBaseMatchers =>

  import cats.data.Validated

  implicit def toValidatedResultMatcher[F, S](result: MatchResult[Validated[F, S]]) =
    new ValidatedResultMatcher(result)

  class ValidatedResultMatcher[F, S](result: MatchResult[Validated[F, S]]) {
    def invalid(f: => F) = result(outer beInvalid f)
    def beInvalid(f: => F) = result(outer beInvalid f)
    def valid(s: => S) = result(outer beValid s)
    def beValid(s: => S) = result(outer beValid s)

    def invalid = result(outer.beInvalid)
    def beInvalid = result(outer.beInvalid)
    def valid = result(outer.beValid)
    def beValid = result(outer.beValid)
  }
}
