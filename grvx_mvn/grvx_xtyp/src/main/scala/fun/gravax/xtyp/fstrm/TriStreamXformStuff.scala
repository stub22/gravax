package fun.gravax.xtyp.fstrm

import cats.effect.IO
import fs2.Stream
import fun.gravax.xtyp.mathy.tridesc.{TriShape, TriShapeXactish}

private trait TriStreamXformStuff

trait TriStrmPipeOps {
	type OurTriErr = String // FIXME:  This should come from MakesTSX or...

	def countTriFailures(inStrmEith: Stream[IO, Either[OurTriErr, TriShapeXactish]]): Stream[IO, Int] = {
		onlyFailures(inStrmEith).fold(0)((prevCnt, nxtErr) => prevCnt + 1)
	}

	def onlyFailures(inStrmEith: Stream[IO, Either[OurTriErr, TriShapeXactish]]): Stream[IO, OurTriErr] = {
		// deprecated: "use `Either.swap.getOrElse` instead", since = "2.13.0")
		// We could use flatMap instead of filter
		inStrmEith.filter(_.isLeft).map(_.left.get)
	}

	def onlyWins(inStrmEith: Stream[IO, Either[OurTriErr, TriShapeXactish]]): Stream[IO, TriShapeXactish] = {
		inStrmEith.filter(_.isRight).map(_.getOrElse(throw new Exception("Bug in filter-map")))
	}

	val myTSC = new TriStreamConsumer {}

	def accumStats(shpStrm: Stream[IO, TriShape]): Stream[IO, TriSetStat] = myTSC.summarizeTriStream(shpStrm)

}
