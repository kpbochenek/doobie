// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.effect.ContextShift
import cats.effect.IO
import cats.free.Free
import cats.implicits.catsSyntaxApplicativeError
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext
import cats.syntax.all._
import doobie.postgres.compositetype.CompositeType
import doobie.postgres.compositetype.Implicits

import java.time.LocalDate
import CompositeTypeSuite._

class CompositeTypeSuite extends munit.FunSuite {

  implicit def contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:world",
    "postgres", ""
  )

  val deleteType: ConnectionIO[Unit] =
    sql"DROP TYPE IF EXISTS CompType".update.run.void

  val createType: ConnectionIO[Unit] = {
    sql"""| CREATE TYPE CompType AS (
          |   p1  text,
          |   p2  int,
          |   p3  boolean,
          |   p4  date,
          |   p5  timestamp,
          |   p6  real,
          |   p7  double precision
          | )
          |""".stripMargin.update.run.void
  }

  val createTable: ConnectionIO[Unit] =
    sql"""| CREATE TEMPORARY TABLE test (
          |   id  integer,
          |   tp  CompType
          | ) ON COMMIT DELETE ROWS
          |""".stripMargin.update.run.void

  def insertAll(elements: List[AllTypesTable]): ConnectionIO[Unit] =
    elements.foldLeft(().pure[ConnectionIO])(
      (acc, v) => acc *> fr"INSERT INTO test VALUES(${v.id}, ${v.a})".update.run.void
    )

  val selectAll: ConnectionIO[List[AllTypesTable]] =
    sql"SELECT * FROM test ORDER BY id".query[AllTypesTable].to[List]

  test("supported types") {
    val inserts = List(
      AllTypesCT("a", 1, true, LocalDate.of(2020, 2, 3), LocalDateTime.of(2020, 1, 1, 1, 1), 3.14f, 3.14),
      AllTypesCT("b", -5, true, LocalDate.of(-3, 1, 12), LocalDateTime.of(-3, 1, 12, 12, 12), -3.14f, -3.14),
      AllTypesCT("c", 0, false, LocalDate.of(0, 12, 24), LocalDateTime.of(0, 12, 24, 23, 10), 12345f, 12345),
      AllTypesCT("d", 123456789, false, LocalDate.of(2034, 8, 5), LocalDateTime.of(2034, 8, 5, 0, 0), 0.0f, 0),
      AllTypesCT("e", 3, false, LocalDate.MIN, LocalDateTime.MIN, 0.0f, 0),
      AllTypesCT("f", 4, false, LocalDate.MAX, LocalDateTime.MAX, 0.0f, 0)
    ).zipWithIndex
      .map(t => AllTypesTable(t._2, t._1))

    val rs = (deleteType *> createType *> createTable *> insertAll(inserts) *> selectAll).transact(xa).unsafeRunSync()

    assertEquals(rs, inserts)
  }
}

object CompositeTypeSuite {
  // all supported types
  final case class AllTypesCT(p1: String, p2: Int, p3: Boolean, p4: LocalDate, p5: LocalDateTime, p6: Float, p7: Double)

  final case class AllTypesTable(id: Int, a: AllTypesCT)

  import Implicits._
  implicit val codec: Meta[AllTypesCT] = CompositeType.instantiate[AllTypesCT]("CompType")


}

