// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.compositetype

import magnolia.CaseClass
import magnolia.Magnolia
import org.postgresql.jdbc.TimestampUtils

import java.time.LocalDate
import java.time.LocalDateTime
import java.util.TimeZone

trait Encoder[T] {
  def encode(element: T): List[String]
}

trait EncoderDerivation {
  // even though it might seem not needed, it is very much needed :)
  type Typeclass[T] = Encoder[T]

  def combine[T](caseClass: CaseClass[Encoder, T]): Encoder[T] = (element: T) =>
    caseClass.parameters.flatMap(p => p.typeclass.encode(p.dereference(element))).toList

  implicit def gen[T]: Encoder[T] = macro Magnolia.gen[T]
}

object Encoder extends EncoderDerivation {
  private final val tsUtils = new TimestampUtils(true, () => TimeZone.getDefault)

  implicit val intEncoder            = simpleTypeEncoder[Int](_.toString)
  implicit val localDateEncoder      = simpleTypeEncoder[LocalDate](tsUtils.toString)
  implicit val localDateTimeEncoder  = simpleTypeEncoder[LocalDateTime](tsUtils.toString)
  implicit val booleanEncoder        = simpleTypeEncoder[Boolean](_.toString)
  implicit val floatEncoder          = simpleTypeEncoder[Float](_.toString)
  implicit val doubleEncoder         = simpleTypeEncoder[Double](_.toString)
  implicit val stringEncoder = simpleTypeEncoder[String](s =>
    "\"" + s
      .replace("""\""", """\\""")
      .replace("\"", "\"\"")
      .replace(""")""", """\)""")
      + "\""
  )

  private def simpleTypeEncoder[T](fn: T => String): Encoder[T] = (element: T) => List(fn(element))

  implicit def oencoder[T](implicit e: Encoder[T]): Encoder[Option[T]] = {
    case Some(element) => e.encode(element)
    case None          => List("")
  }

  def concat(values: List[String]): String = {
    values.mkString("(", ",", ")")
  }
}
