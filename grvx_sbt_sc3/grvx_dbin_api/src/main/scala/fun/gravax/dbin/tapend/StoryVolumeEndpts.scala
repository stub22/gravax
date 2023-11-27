package fun.gravax.dbin.tapend

import sttp.tapir.*
import Library.*
import cats.effect.IO
import StoryVolumeEndpts.Story
import io.circe.generic.auto.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.metrics.prometheus.PrometheusMetrics
import sttp.tapir.swagger.bundle.SwaggerInterpreter

import scala.collection.immutable.List


object StoryVolumeEndpts {
	// The AnyVal helps us treat Story as a String, but seems to get in the way when we want to circe-encode the class
	case class Story(title: String) extends AnyVal

	case class Tale(theme : String, pages : Int, relev : BigDecimal)

	case class Volume(volName : String, tales : Seq[Tale])

	val goodVol01 = Volume("goodv-01", Seq(Tale("spooky", 150, BigDecimal(2.01)), Tale("sad", -10, BigDecimal(-14.44)),
			Tale("funky", 77, BigDecimal(1999)), Tale("glad", 7, BigDecimal(4))))

	// Extracts the title from a Story
	val pubEnd_storyInfo: PublicEndpoint[Story, Unit, String, Any] = endpoint.get
			.in("story")
			.in(query[Story]("title"))
			.out(stringBody)

	val storyServEndpt: ServerEndpoint[Any, IO] = pubEnd_storyInfo.serverLogicSuccess(stry => IO.pure(s"Made a story for you: ${stry}"))

	val pubEnd_volumeListing: PublicEndpoint[Unit, Unit, List[Tale], Any] = endpoint.get
			.in("volume" / "list" / "all")
			.out(jsonBody[List[Tale]])

	val volumeListingServEndpt: ServerEndpoint[Any, IO] = pubEnd_volumeListing.serverLogicSuccess(_ => IO.pure(goodVol01.tales.toList))

	val pubEnd_volWithMetadata: PublicEndpoint[Unit, Unit, Volume, Any] = endpoint.get
			.in("volume" / "wmet")
			.out(jsonBody[Volume])

	val volWithMetaServEndpt: ServerEndpoint[Any, IO] = pubEnd_volWithMetadata.serverLogicSuccess(_ => IO.pure(goodVol01))

	val storyVolEndpts = List(storyServEndpt, volumeListingServEndpt, volWithMetaServEndpt)
}


/*

E:\_emnt\shallow_experiments\tapgen\src\main\scala\com\appstract\DistBinCalcEndpts.scala:33:18
cannot reduce summonFrom with
patterns :  case given encodeA @ _:io.circe.Encoder[com.appstract.DistBinCalcEndpts.Story]
			case given evidence$1 @ _:deriving.Mirror.Of[com.appstract.DistBinCalcEndpts.Story]
			.out(jsonBody[List[Story]])



E:\_emnt\shallow_experiments\tapgen\src\main\scala\com\appstract\DistBinCalcEndpts.scala:32:17
No given instance of type io.circe.Encoder[Seq[com.appstract.DistBinCalcEndpts.Story]] was found for an implicit parameter of method jsonBody in trait TapirJsonCirce.
I found:

	io.circe.Encoder.encodeIterable[com.appstract.DistBinCalcEndpts.Story, Seq](
	io.circe.Encoder.importedEncoder[com.appstract.DistBinCalcEndpts.Story](
		io.circe.generic.auto.deriveEncoder[com.appstract.DistBinCalcEndpts.Story](
		/* missing */
			summon[deriving.Mirror.Of[com.appstract.DistBinCalcEndpts.Story]]
		)
	),
	???)

But Failed to synthesize an instance of type deriving.Mirror.Of[com.appstract.DistBinCalcEndpts.Story]:
	* class Story is not a generic product because it is a value class
	* class Story is not a generic sum because it is not a sealed class.

One of the following imports might make progress towards fixing the problem:

import io.circe.Encoder.AsArray.importedAsArrayEncoder
import io.circe.Encoder.AsObject.importedAsObjectEncoder
import io.circe.Encoder.AsRoot.importedAsRootEncoder

			.out(jsonBody[Seq[Story]])
*/