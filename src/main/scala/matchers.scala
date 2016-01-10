package org.specs2.cats

import org.specs2.matcher._
import org.specs2.text.Quote._
import org.specs2.execute.{ Failure, Result }

trait CatsMatchers
  extends XorBaseMatchers
  with XorBeHaveMatchers

private[specs2] trait XorBaseMatchers {
  import cats.data.Xor

  def beXorRight[T](t: => T) =
    new Matcher[Xor[_, T]] {
      def apply[S <: Xor[_, T]](value: Expectable[S]) = {
        val expected = t
        result(
          value.value == Xor.Right(t),
          s"${value.description} is Right with value ${q(expected)}",
          s"${value.description} is not Right with value ${q(expected)}",
          value
        )
      }
    }

  def beXorRight[T] =
    new Matcher[Xor[_, T]] {
      def apply[S <: Xor[_, T]](value: Expectable[S]) = {
        result(
          value.value.isRight,
          s"${value.description} is Right",
          s"${value.description} is not Right",
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
          s"${value.description} is Left with value ${q(expected)}",
          s"${value.description} is not Left with value ${q(expected)}",
          value
        )
      }
    }

  def beXorLeft[T] = new Matcher[Xor[T, _]] {
    def apply[S <: Xor[T, _]](value: Expectable[S]) = {
      result(
        value.value.isLeft,
        s"${value.description} is Left",
        s"${value.description} is not Left",
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
          s"${value.description} is Left and ${res.message}",
          s"${value.description} is Left but  ${res.message}",
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

