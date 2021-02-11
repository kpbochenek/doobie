// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.compositetype

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import language.experimental.macros
import magnolia._
import org.postgresql.jdbc.TimestampUtils

import java.sql.Date
import java.time.LocalDate
import java.time.LocalDateTime
import java.util.Calendar
import java.util.TimeZone

trait Decoder[T] {
  def decode(values: List[Option[String]]): (T, List[Option[String]])
}

trait DecoderDerivation {
  // even though it might seem not needed, it is very much needed :)
  type Typeclass[T] = Decoder[T]

  def combine[T](ctx: CaseClass[Decoder, T]): Decoder[T] = new Decoder[T] {
    def decode(values: List[Option[String]]): (T, List[Option[String]]) = {
      var valuesRemaining = values
      val fieldValues = ctx.parameters.map { param =>
        val (elem, remaining) = param.typeclass.decode(valuesRemaining)
        valuesRemaining = remaining
        elem
      }
      (ctx.rawConstruct(fieldValues), valuesRemaining)
    }

  }

  implicit def gen[T]: Decoder[T] = macro Magnolia.gen[T]
}

object Decoder extends DecoderDerivation with SimpleTypeDecoder {

  private final val tsUtils = new TimestampUtils(true, () => TimeZone.getDefault)
//  val cal = Calendar.getInstance(TimeZone.getDefault)
//  val cal = tsUtils.getSharedCalendar(null);

  implicit val intDecoder            = simpleTypeDecoder[Int](Integer.parseInt)
  implicit val booleanDecoder        = simpleTypeDecoder[Boolean](_ == "t")
  implicit val floatDecoder          = simpleTypeDecoder[Float](_.toFloat)
  implicit val localDateDecoder      = simpleTypeDecoder[LocalDate](s => tsUtils.toLocalDateTime(s.filter(_ != '"')).toLocalDate)
  implicit val localDateTimeDecoder  = simpleTypeDecoder[LocalDateTime](s => tsUtils.toLocalDateTime(s.filter(_ != '"')))
  implicit val doubleDecoder         = simpleTypeDecoder[Double](_.toDouble)
  implicit val stringDecoder =
    simpleTypeDecoder[String](s => (if (s.startsWith("\"") && s.endsWith("\"")) s.substring(1, s.length - 1) else s).replace("\\\\", "\\"))

  private def simpleTypeDecoder[T](fn: String => T): Decoder[T] =
    (values: List[Option[String]]) => {
      val headOption = values.headOption.getOrElse(
        throw new IllegalArgumentException(s"Values ${values} must not be empty during composite type decoding.")
      )
      val head =
        headOption.getOrElse(throw new IllegalArgumentException(s"Values ${headOption} must not be empty during composite type decoding."))
      (fn(head), values.tail)
    }

  def split(value: String): List[Option[String]] = {
    val result                  = ListBuffer[Option[String]]()
    def addAcc(s: String): Unit = if (s.isEmpty) result += None else result += Some(s)

    @tailrec def recur(pos: Int, insideQuote: Boolean, acc: String): Unit = {
      if (pos == value.length - 1) {
        // reaching end of record string representation
        addAcc(acc)
      } else if (insideQuote && value.charAt(pos) == '"' && value.charAt(pos + 1) == '"') {
        // inside quoted text and found escaped "  (escaping " is by adding another ". E.g. "tom loves ""crazy"" books"
        recur(pos + 2, insideQuote, acc + '"')
      } else if (value.charAt(pos) == ',' && !insideQuote) {
        // encountered ',' which is a delimiter of fields in record
        addAcc(acc)
        recur(pos + 1, false, "")
      } else if (!insideQuote && value.charAt(pos) == '"') {
        // because previously we checked if there is "" then it must be single " - opening of quote
        recur(pos + 1, true, "\"")
      } else if (insideQuote && value.charAt(pos) == '"') {
        // because previously we checked if there is "" then it must be single " - closing of quote
        recur(pos + 1, false, acc + '"')
      } else {
        recur(pos + 1, insideQuote, acc + value.charAt(pos))
      }
    }
    // skip opening '('
    recur(1, false, "")
    result.toList
  }
}

trait SimpleTypeDecoder {

  implicit def odecoder[T](implicit d: Decoder[T]): Decoder[Option[T]] = (values: List[Option[String]]) => {
    val head = values.headOption.getOrElse(
      throw new IllegalArgumentException(s"Values ${values} must not be empty during composite type decoding.")
    )
    val element = if (head.isEmpty) None else Some(d.decode(List(head))._1)
    (element, values.tail)
  }
}
