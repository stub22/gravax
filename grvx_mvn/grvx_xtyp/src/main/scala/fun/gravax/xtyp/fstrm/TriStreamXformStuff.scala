package fun.gravax.xtyp.fstrm

import cats.effect.{Clock, IO}
import fs2.Stream
import fs2.timeseries.TimeStamped
import fun.gravax.xtyp.mathy.tridesc.{TriShape, TriShapeXactish}

import scala.concurrent.duration.FiniteDuration

private trait TriStreamXformStuff

trait TriStrmPipeOps {
	type OurTriErr = String // FIXME:  This should come from MakesTSX or...

	def countAndPrintTriFailuresButEmitNothing(inStrmEith: Stream[IO, Either[OurTriErr, TriShapeXactish]]): Stream[IO, Nothing] = {
		countTriFailures(inStrmEith).debug(logger = txt => {println(s"countAndPrintTriFailuresButEmitNothing debug: ${txt}")}).drain
	}
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

	def stampyShapes(shpStrm: Stream[IO, TriShape]): Stream[IO, TimeStamped[TriShape]] = {
	//	val z = Clock.create[IO]
		val ioClck = IO.asyncForIO
		val stmpd: Stream[IO, TimeStamped[TriShape]] = shpStrm.evalMap(shp => TimeStamped.now(shp)(ioClck, ioClck))
		stmpd
	}

	def somePullStuff(shpStrm: Stream[IO, TriShape]) : Unit = {
		val p = shpStrm.pull

		val z = shpStrm.compile.drain.background
	}

}
trait SomeUtilityStreams {
	val myRealTimeJob: IO[FiniteDuration] = IO.realTime

	// Make a time-reporting stream that keeps track of its own start and end times.
	// By .flatMapping this stream we weill
	def timedStream(name : String) : Stream[IO, FiniteDuration] = Stream.bracket(myRealTimeJob)((startDur: FiniteDuration) => IO.defer(
		myRealTimeJob.map(endDur => {
			val durDiff = endDur - startDur
			println(s"${name} startDur=${startDur}, endDur=${endDur}, durDiff=${durDiff}")
		})
	))
}
/*
"SyncIO is like IO, but it does not support Concurrent, Temporal, or Async
you can unsafeRun() a SyncIO, or you can convert it .to[IO]"

https://discord.com/channels/632277896739946517/632310980449402880/966754369930420294


Stream.resource(r) and Stream.bracket(acquire)(release) and similar variants extend resource life time over flatMap
but otherwise release the acquired resource as soon as possible.
That is, in the expression Stream.resource(r).flatMap(f) ++ s the resource acquired by r is active during the stream
created by f but is released before s

 */