package fun.gravax.zaxlam.zex

import zio.json._

final case class CustomEvent(message: String)

object CustomEvent {
  implicit val decoder: JsonDecoder[CustomEvent] = DeriveJsonDecoder.gen[CustomEvent]
}
