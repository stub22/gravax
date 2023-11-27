package fun.gravax.dbin.tapend

import sttp.tapir.*
import cats.effect.IO
import io.circe.generic.auto.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.metrics.prometheus.PrometheusMetrics
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import fun.gravax.dbin.model.{Bin, TestBins}


object DbinEndpts {
	val pubEnd_binSeq: PublicEndpoint[Unit, Unit, Seq[Bin], Any] = endpoint.get
			.in("bin" / "list" / "all")
			.out(jsonBody[Seq[Bin]])

	val binSeqServEndpt: ServerEndpoint[Any, IO] = pubEnd_binSeq.serverLogicSuccess(_ => IO.pure(TestBins.bins))

	val binEndpts = List(binSeqServEndpt)
}
