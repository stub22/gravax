package fun.gravax.dbin.tapend

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.*
import fs2.{Chunk, Stream}
import org.http4s.HttpRoutes

import org.http4s.server.Router
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3._
import sttp.model.HeaderNames
import sttp.tapir._
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.nio.charset.StandardCharsets
import scala.concurrent.duration._

// https://github.com/softwaremill/tapir/issues/367
object StreamingHttp4sFs2Server extends IOApp {
	// corresponds to: GET /receive?name=...
	// We need to provide both the schema of the value (for documentation), as well as the format (media type) of the
	// body. Here, the schema is a `string` (set by `streamTextBody`) and the media type is `text/plain`.
	val streamingEndpoint: PublicEndpoint[Unit, Unit, (Long, Stream[IO, Byte]), Fs2Streams[IO]] =
	endpoint.get
			.in("receive")
			.out(header[Long](HeaderNames.ContentLength))
			.out(streamTextBody(Fs2Streams[IO])(CodecFormat.TextPlain(), Some(StandardCharsets.UTF_8)))

	// converting an endpoint to a route (providing server-side logic)
	val streamingRoutes: HttpRoutes[IO] =
		Http4sServerInterpreter[IO]().toRoutes(streamingEndpoint.serverLogicSuccess { _ =>
			val size = 100L
			Stream
					.emit(List[Char]('a', 'b', 'c', 'd'))
					.repeat
					.flatMap(list => Stream.chunk(Chunk.seq(list)))
					.debugChunks(chnk =>"flatMapped: " + chnk)
					.metered[IO](100.millis)
					.debugChunks(chnk =>"metered: " + chnk)
					.take(size)
					.covary[IO]
					.map(_.toByte)
					.pure[IO]
					.map(s => (size, s))
		})

	override def run(args: List[String]): IO[ExitCode] = {
		runWithEmber
	}
	def runWithEmber = {
		import com.comcast.ip4s.{Host, Port, port}
		import org.http4s.ember.server.EmberServerBuilder
		val p = port"8080"
		EmberServerBuilder
				.default[IO]
				.withHost(Host.fromString("localhost").get)
				.withPort(p)
				.withHttpApp(Router("/" -> streamingRoutes).orNotFound)
				.build
				.use { server =>
					IO {
						val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()
						val result: String = basicRequest.response(asStringAlways).get(uri"http://localhost:8080/receive").send(backend).body
						println("Got result: " + result)

						assert(result == "abcd" * 25)
					}

				}
				.as(ExitCode.Success)
	}
	def origRunWithBlaze(args: List[String]) = {
		// starting the server
/*  import org.http4s.blaze.server.BlazeServerBuilder
		BlazeServerBuilder[IO]
				.bindHttp(8080, "localhost")
				.withHttpApp(Router("/" -> streamingRoutes).orNotFound)
				.resource
				.use { _ =>
					IO {
						val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()
						val result: String = basicRequest.response(asStringAlways).get(uri"http://localhost:8080/receive").send(backend).body
						println("Got result: " + result)

						assert(result == "abcd" * 25)
					}
				}
				.as(ExitCode.Success)
*/
	}
}