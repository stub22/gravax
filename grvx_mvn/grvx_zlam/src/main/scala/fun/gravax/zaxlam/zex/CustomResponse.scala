package fun.gravax.zaxlam.zex

import zio.json._

final case class CustomResponse(message: String)

object CustomResponse {
  implicit val encoder: JsonEncoder[CustomResponse] = DeriveJsonEncoder.gen[CustomResponse]
}
