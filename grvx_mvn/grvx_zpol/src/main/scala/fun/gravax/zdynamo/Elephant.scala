package fun.gravax.zdynamo

import zio.dynamodb.ProjectionExpression
import zio.schema.{DeriveSchema, Schema}

final case class Elephant(id: String, email: String)

object Elephant {
  implicit val schema: Schema.CaseClass2[String, String, Elephant] =
    DeriveSchema.gen[Elephant]
  val (id, email)                                                  = ProjectionExpression.accessors[Elephant]
}
