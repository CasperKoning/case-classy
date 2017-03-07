/* -
 * Case Classy [classy-tests]
 */

package classy

import scala.Predef._

//import _root_.cats.instances.all._
import _root_.cats.kernel.laws._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.listOf
import org.scalacheck.Prop._

import org.scalacheck.derive._
import org.scalacheck.{ Shapeless => blackMagic }

import cats._

class DecodeErrorProperties extends Properties("DecodeError") {
  import DecodeError._

  val genLeaf: Gen[DecodeError] = {
    import blackMagic._
    MkArbitrary[LeafDecodeError].arbitrary.arbitrary
  }

  property("and two leaf errors") =
    forAll(genLeaf, genLeaf)((a, b) => DecodeError.and(a, b) ?= And(a, b))

  property("&& two leaf errors") =
    forAll(genLeaf, genLeaf)((a, b) => a && b ?= And(a, b))

  property("or two leaf errors") =
    forAll(genLeaf, genLeaf)((a, b) => DecodeError.or(a, b) ?= Or(a, b))

  property("|| two leaf errors") =
    forAll(genLeaf, genLeaf)((a, b) => a || b ?= Or(a, b))

  property("and many leaf errors") =
    forAll(listOf(genLeaf))(errors => errors.length >= 2 ==> (
      errors.reduce(DecodeError.and) ?= And(errors.head, errors.tail)))

  property("&& many leaf errors") =
    forAll(listOf(genLeaf))(errors => errors.length >= 2 ==> (
      errors.reduce(_ && _) ?= And(errors.head, errors.tail)))

  property("or many leaf errors") =
    forAll(listOf(genLeaf))(errors => errors.length >= 2 ==> (
      errors.reduce(DecodeError.or) ?= Or(errors.head, errors.tail)))

  property("|| many leaf errors") =
    forAll(listOf(genLeaf))(errors => errors.length >= 2 ==> (
      errors.reduce(_ || _) ?= Or(errors.head, errors.tail)))

  property("atPath") =
    forAll(
      arbitrary[String] :| "path",
      arbitrary[String] :| "missing path"
    )((path, missingPath) =>
        Missing.atPath(missingPath).atPath(path) ?=
          AtPath(path, AtPath(missingPath, Missing)))

  property("atIndex") =
    forAll(
      arbitrary[Int] :| "index",
      arbitrary[String] :| "missing path"
    )((index, missingPath) =>
        Missing.atPath(missingPath).atIndex(index) ?=
          AtIndex(index, AtPath(missingPath, Missing)))

  property("AtPath.deepError") =
    forAll(
      genLeaf :| "deep error",
      arbitrary[String] :| "paths head",
      arbitrary[List[String]] :| "paths tail"
    )((error, pathHead, pathTail) =>
        pathTail.foldLeft(
          error.atPath(pathHead)
        )(_ atPath _).deepError ?= error)

  {
    import blackMagic._
    implicit val arbitraryDecodeError = MkArbitrary[DecodeError].arbitrary
    include(GroupLaws[DecodeError].monoid.all)
  }
}
