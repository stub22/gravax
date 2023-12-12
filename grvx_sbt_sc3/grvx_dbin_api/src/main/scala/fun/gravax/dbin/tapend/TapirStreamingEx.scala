package fun.gravax.dbin.tapend

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.*
import fs2.{Chunk, Stream}
import org.http4s.HttpRoutes
import org.http4s.server.Router
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.*
import sttp.model.HeaderNames
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.*

// Original example used Blaze, which we replaced here with Ember to avoid the Blaze dependency.
// https://github.com/softwaremill/tapir/issues/367

trait StreamyFuncs {
	def buildStreamEff: IO[(Long, Stream[IO, Byte])] = {
		val size = 100L
		val sbuildEff: IO[(Long, Stream[IO, Byte])] = Stream
				.emit(List[Char]('a', 'b', 'c', 'd'))
				.repeat
				.flatMap(list => Stream.chunk(Chunk.seq(list)))
				.debugChunks(chnk => "flatMapped: " + chnk)
				.metered[IO](100.millis)
				.debugChunks(chnk => "metered: " + chnk)
				.take(size)
				.covary[IO]
				.map(_.toByte)
				.pure[IO]
				.map(s => (size, s))
		sbuildEff
	}

	def buildClocky = {
		val seconds = Stream.awakeEvery[IO](1.second)
	}
}
object StreamyEnds {
	// corresponds to: GET /receive?name=...
	// We need to provide both the schema of the value (for documentation), as well as the format (media type) of the
	// body. Here, the schema is a `string` (set by `streamTextBody`) and the media type is `text/plain`.
	val streamingEndpoint: PublicEndpoint[Unit, Unit, (Long, Stream[IO, Byte]), Fs2Streams[IO]] =
	endpoint.get
			.in("receive")
			.out(header[Long](HeaderNames.ContentLength))
			.out(streamTextBody(Fs2Streams[IO])(CodecFormat.TextPlain(), Some(StandardCharsets.UTF_8)))

	val streamyFuncs = new StreamyFuncs {}

	// Gnarly type: Full[Unit, Unit, Unit, Unit, (Long, Stream[IO, Byte]), Fs2Streams[IO], IO]
	// This is a ServerEndpoint, but not easily added into conforming list of endpoints.
	// Seems easier to turn it directly into an HttpRoute.
	val realizedStreamyEndy = streamingEndpoint.serverLogicSuccess { _ => streamyFuncs.buildStreamEff }

	// converting an endpoint to a route (providing server-side logic)
	val streamingRoutes: HttpRoutes[IO] = Http4sServerInterpreter[IO]().toRoutes(realizedStreamyEndy)
}
object StreamingHttp4sFs2Server extends IOApp {
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
				.withHttpApp(Router("/" -> StreamyEnds.streamingRoutes).orNotFound)
				.build
				.use { server =>
					IO {
						val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()
/*

Reads the response body as an Either[String, String], where Left is used if the status code is non-2xx, and Right otherwise.
val basicRequest: RequestT[Empty, Either[String, String], Any] =

Specifies the target type to which the response body should be read. Note that this replaces any previous
* specifications, which also includes any previous `mapResponse` invocations.
def response[T2, R2](ra: ResponseAs[T2, R2]): RequestT[U, T2, R with R2]

 Use the `utf-8` charset by default, unless specified otherwise in the response headers.
def asStringAlways: ResponseAs[String, Any] = asStringAlways(Utf8)

We could instead use:
  def asByteArrayAlways
  def asStreamAlways
  def asWebSocketAlways

*/
						val result: String = basicRequest.response(asStringAlways).get(uri"http://localhost:8080/receive").send(backend).body
						println("Got result: " + result)

						assert(result == "abcd" * 25)
					}

				}
				.as(ExitCode.Success)
	}

}