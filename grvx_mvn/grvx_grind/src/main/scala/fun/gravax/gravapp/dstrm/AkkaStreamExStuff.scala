package fun.gravax.gravapp.dstrm

private trait AkkaStreamExStuff

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Sink, Source}

import scala.concurrent.Future

/**
 * Code copied from:
 * https://github.com/pbernet/akka_streams_tutorial/blob/master/src/main/scala/sample/stream/BasicTransformation.scala
 *
 * Inspired by:
 * https://stackoverflow.com/questions/35120082/how-to-get-started-with-akka-streams
 *
 */
object BasicTransformation {
	implicit val system: ActorSystem = ActorSystem()
	import system.dispatcher	// Used in implicit arg to Future.onComplete

	def main(args: Array[String]): Unit = {
		val text =
			"""|Lorem Ipsum is simply dummy text of the printing and typesetting industry.
			   |Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
			   |when an unknown printer took a galley of type and scrambled it to make a type
			   |specimen book.""".stripMargin

		val source: Source[String, NotUsed] = Source.fromIterator(() => text.split("\\s").iterator)
		val sink: Sink[String, Future[Done]] = Sink.foreach[String](println)
		val flow: Flow[String, String, NotUsed] = Flow[String].map(x => x.toUpperCase)
		val result: Future[Done] = source.via(flow).runWith(sink)
		result.onComplete(_ => system.terminate())
	}
}