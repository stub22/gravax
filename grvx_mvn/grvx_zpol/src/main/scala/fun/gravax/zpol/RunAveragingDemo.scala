package fun.gravax.zpol

import zio.stream.{UStream, ZSink, ZStream}
import zio.{UIO, ZIO, ZIOAppDefault, Random => ZRandom}

import java.io.IOException
import java.math.{MathContext, RoundingMode}


object RunAveragingDemo extends ZIOAppDefault {

	def run = myAppLogic

	val myAppLogic: ZIO[Any, IOException, Unit] = {
		val demoFeatures = new AveragingDemoFeatures {}
		for {
			_	<- ZIO.log("RunAveragingDemo BEGIN")
			zs  <- demoFeatures.sumPositiveInts(1000)
			_	<- ZIO.log(s"Sum result: ${zs}")
			dn <- demoFeatures.dumNum
			_	<- ZIO.log(s"Dumb rando: ${dn}")
			adn <- demoFeatures.avgDumNum
			_	<- ZIO.log(s"Average dumNum = ${adn}")
			padn <- demoFeatures.parAvgDumNum
			_	<- ZIO.log(s"Par-Average dumNum = ${padn}")
		} yield ()
	}



}
trait AveragingDemoFeatures {
	def sumPositiveInts(posInt : Int) : UIO[Int] = {
		val rangeToSum = 1 to posInt
		val stream: ZStream[Any, Nothing, Int] = ZStream.fromIterable(rangeToSum)
		val sink: ZSink[Any, Nothing, Int, Nothing, Int] = ZSink.sum[Int]
		val sum: ZIO[Any, Nothing, Int] = stream.run(sink)
		val sumUIO : UIO[Int] = sum
		sumUIO
	}

	def gaussianBD(zrnd : ZRandom, mathCtx : MathContext,  mean : BigDecimal, stdDev : BigDecimal): UIO[BigDecimal] = {
		val stdNumEff: UIO[Double] = zrnd.nextGaussian
		stdNumEff.map(num => {
			// https://blogs.oracle.com/javamagazine/post/four-common-pitfalls-of-the-bigdecimal-class-and-how-to-avoid-them
			val stdBD = BigDecimal(num, mathCtx)
			val scaledAndShiftedBD = stdBD.*(stdDev).+(mean)
			scaledAndShiftedBD
		})
	}
	def dumNum : UIO[BigDecimal] = {
		val mean = BigDecimal("-5.0")
		val dev = BigDecimal("4.5")
		val mathCtx = new MathContext(8, RoundingMode.HALF_UP)
		gaussianBD(ZRandom.RandomLive, mathCtx, mean, dev)
	}
	val myZeroAccumPair : (Int, BigDecimal) = (0, BigDecimal("0.0"))
	lazy val myAccumSink: ZSink[Any, Nothing, BigDecimal, Nothing, (Int, BigDecimal)] = {

		// foldLeft[In, S](z: => S)(f: (S, In) => S)
		val accumSink = ZSink.foldLeft[BigDecimal, (Int, BigDecimal)](myZeroAccumPair)((prevStPair, nxtBD) => {
			(prevStPair._1 + 1, prevStPair._2.+(nxtBD))
		})
		accumSink
	}
	lazy val myAvgSink = myAccumSink.map(pair => {pair._2./(pair._1)})

	def avgDumNum : UIO[BigDecimal] = {
		val strm: UStream[BigDecimal] = ZStream.repeatZIO(dumNum)
		val shortStrm = strm.take(500)
		val shortAvg = shortStrm.run(myAvgSink)
		shortAvg
	}
	def parAvgDumNum : UIO[BigDecimal] = {
		val origStrm: UStream[BigDecimal] = ZStream.repeatZIO(dumNum)
		// Tag each record with a sequence number that we can use later in .groupBy.
		val strmWithIdx: UStream[(BigDecimal, Long)] = origStrm.zipWithIndex
		val finiteStrm = strmWithIdx.take(5000)
		val numGroups = 8
		// grouped : ZStream.GroupBy[Any, Nothing, Long, (BigDecimal, Long)]
		val grouped = finiteStrm.groupByKey(pair => pair._2 % numGroups, 16)
		val mergedResultStrm: UStream[(Int, BigDecimal)] = grouped.apply((key, strm) => {
			val accumZIO: UIO[(Int, BigDecimal)] = strm.map(_._1).run(myAccumSink)
			val azs: UStream[(Int, BigDecimal)] = ZStream.fromZIO(accumZIO)
			azs
		})
		val dbgMRS = mergedResultStrm.debug("parAvgDumNum: partial result")
		val accumCombSink = ZSink.foldLeft[(Int, BigDecimal), (Int, BigDecimal)](myZeroAccumPair)((prevStPair, nxtAccumPair) => {
			(prevStPair._1 + nxtAccumPair._1, prevStPair._2.+(nxtAccumPair._2))
		})
		val combAvgSink = accumCombSink.map(pair => {pair._2./(pair._1)})
		val combAvgUIO: UIO[BigDecimal] = dbgMRS.run(combAvgSink)
		combAvgUIO
	}



}
/*
https://zio.dev/reference/stream/
type Stream[+E, +A] = ZStream[Any, E, A]
type UStream[+A]    = ZStream[Any, Nothing, A]

Streams and Sinks are duals in category theory. One produces values, and the other one consumes them.
They are mere images of each other. They both have to exist. A streaming library cannot be complete unless it has
streams and sinks. That is why ZIO has a sort of better design than FS2 because FS2 has a stream, but it doesn't
have a sink. Its Sink is just faked. It doesn't actually have a real sink.
ZIO has a real sink, and we can compose them to generate new sinks.

https://zio.dev/reference/core/zio/task/
type Task[+A] = ZIO[Any, Throwable, A]
So a Task is equal to a ZIO that doesn't need any requirement, and may fail with a Throwable, or succeed with an A value.

https://zio.dev/reference/core/zio/uio/
type UIO[+A] = ZIO[Any, Nothing, A]

So UIO is equal to a ZIO that doesn't need any requirement (because it accepts Any environment) and that cannot fail
(because in Scala the Nothing type is uninhabitable, i.e. there can be no actual value of type Nothing).
It succeeds with A.

ZIO values of type UIO[A] are considered infallible. Values of this type may produce an A, but will never fail.

 */