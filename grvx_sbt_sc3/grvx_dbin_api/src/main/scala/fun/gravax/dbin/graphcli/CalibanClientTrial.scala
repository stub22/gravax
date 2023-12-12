package fun.gravax.dbin.graphcli


import sttp.client3.*
// import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio.{Chunk, IO, Promise, RIO, Task, UIO, ZIO}
import caliban.*
import caliban.client._
import caliban.client.Operations.RootQuery
class CalibanClientTrial {
/*
	val portfQuery = // : SelectionBuilder[RootQuery, List[Client.Portfolio]] =
		Client.Query.portfolios
	//	Query.characters(Origin.MARS) {
	//		character
//		}
	def okGo = {
		HttpClientZioBackend().flatMap { implicit backend =>
			val serverUrl = uri"http://localhost:4000"
			val result: Task[List[Client.Portfolio]] =
				portfQuery.toRequest(serverUrl).send().map(_.body).absolve
			// ...
		}
	}
*/
}

