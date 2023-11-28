package fun.gravax.dbin.tapend

class JunkIgnore
/*
import Endpoints.{*, given}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client3.testing.SttpBackendStub
import sttp.client3.{UriContext, basicRequest}
import sttp.tapir.server.stub.TapirStubInterpreter

import Library.*
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.circe.generic.auto.*
import sttp.client3.circe.*
import sttp.tapir.integ.cats.effect.CatsMonadError

class EndpointsSpec extends AnyFlatSpec with Matchers with EitherValues:

  val myHelpers = new EndpointTestHelpers

  it should "return hello message" in {
    // given
    val backendStub = TapirStubInterpreter(SttpBackendStub(new CatsMonadError[IO]()))
      .whenServerEndpointRunLogic(helloServerEndpoint)
      .backend()

    // when
    val response = basicRequest
      .get(uri"http://test.com/hello?name=adam")
      .send(backendStub)

    // then

    response.map(_.body.value shouldBe "Hello adam").unwrap
  }

  it should "list available books" in {
    // given
    val backendStub = TapirStubInterpreter(SttpBackendStub(new CatsMonadError[IO]()))
      .whenServerEndpointRunLogic(booksListingServerEndpoint)
      .backend()

    // when
    val response = basicRequest
      .get(uri"http://test.com/books/list/all")
      .response(asJson[List[Book]])
      .send(backendStub)

    // then
    response.map(_.body.value shouldBe books).unwrap
  }

  extension [T](t: IO[T]) def unwrap: T = t.unsafeRunSync()
*/