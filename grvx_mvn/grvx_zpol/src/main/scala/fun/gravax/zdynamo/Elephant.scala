package fun.gravax.zdynamo

import zio.dynamodb.ProjectionExpression
import zio.schema.{DeriveSchema, Schema}

// Adapted from https://github.com/zio/zio-dynamodb/blob/series/2.x/examples/src/main/scala/zio/dynamodb/examples/model/Elephant.scala
final case class Elephant(id: String, email: String)

object Elephant {
  implicit val schema: Schema.CaseClass2[String, String, Elephant] =
    DeriveSchema.gen[Elephant]
  val (id, email)                                                  = ProjectionExpression.accessors[Elephant]
}
