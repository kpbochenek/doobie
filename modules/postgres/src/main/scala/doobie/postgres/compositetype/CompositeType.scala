// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.compositetype

import doobie.Meta
import org.postgresql.util.PGobject
import org.tpolecat.typename.TypeName

object Implicits {
  implicit def compType[T](implicit encoder: Encoder[T], decoder: Decoder[T]): CompositeType[T] = new CompositeType[T] {
    override def decode(value: String): T  = decoder.decode(Decoder.split(value))._1
    override def encode(entity: T): String = Encoder.concat(encoder.encode(entity))
  }

}

trait CompositeType[T] {
  def decode(value: String): T
  def encode(entity: T): String
}

object CompositeType {
  private def encodePgobject(typeName: String, value: String): PGobject = {
    val o = new PGobject()
    o.setType(typeName)
    o.setValue(value)
    o
  }

  def instantiate[T: TypeName](typeName: String)(implicit codec: CompositeType[T]): Meta[T] = Meta.Advanced
    .other[PGobject](typeName)
    .timap[T](pgObject => codec.decode(pgObject.getValue))(entity => encodePgobject(typeName, codec.encode(entity)))
}
