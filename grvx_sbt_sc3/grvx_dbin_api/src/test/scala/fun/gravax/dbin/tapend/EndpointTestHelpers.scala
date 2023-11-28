package fun.gravax.dbin.tapend

import sttp.tapir.server.stub.TapirStubInterpreter
import sttp.client3.testing.SttpBackendStub
import sttp.client3.{Response, ResponseException, SttpBackend, UriContext, basicRequest}
import sttp.tapir.integ.cats.effect.CatsMonadError
import cats.effect.IO
import sttp.client3.circe.*
import io.circe.generic.auto.*
import Endpoints.{booksListingServerEndpoint, helloServerEndpoint, given}
import Library.Book
import io.circe

// Importing .* brings in all NON-givens
// https://docs.scala-lang.org/scala3/book/packaging-imports.html

class EndpointTestHelpers {

	def doX = {
		val backendStub: SttpBackend[IO, Nothing] = TapirStubInterpreter(SttpBackendStub(new CatsMonadError[IO]()))
				.whenServerEndpointRunLogic(helloServerEndpoint)
				.backend()

		// when
		val response: IO[Response[Either[String, String]]] = basicRequest
				.get(uri"http://test.com/hello?name=adam")
				.send(backendStub)

		// then
		// .value comes from  org.scalatest.EitherValuable
		// response.map(_.body.value shouldBe "Hello adam").unwrap
	}

	def doY = {
		// given
		val backendStub = TapirStubInterpreter(SttpBackendStub(new CatsMonadError[IO]()))
				.whenServerEndpointRunLogic(booksListingServerEndpoint)
				.backend()

		// when
		val response: IO[Response[Either[ResponseException[String, circe.Error], List[Book]]]] = basicRequest
				.get(uri"http://test.com/books/list/all")
				.response(asJson[List[Book]])
				.send(backendStub)
	}

}
