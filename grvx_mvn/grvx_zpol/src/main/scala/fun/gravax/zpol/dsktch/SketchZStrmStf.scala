package fun.gravax.zpol.dsktch

import zio.stream.{ZSink, ZStream}
import zio.{UIO, ZIO, ZIOAppDefault}
import zio.{Random => ZRandom}

import java.io.IOException
import java.math.{MathContext, RoundingMode}


private trait SketchZStrmStf

object RunDSktchZstrmDemo extends ZIOAppDefault {

	def run = myAppLogic

	val myAppLogic: ZIO[Any, IOException, Unit] = {
		val demoFeatures = new ZStrmDemoFeatures {}
		for {
			_	<- ZIO.log("RunDSktchZstrmDemo BEGIN")
			zs  <- demoFeatures.sumPositiveInts(1000)
			_	<- ZIO.log(s"Sum result: ${zs}")
			dn <- demoFeatures.dumNum
			_	<- ZIO.log(s"Dumb rando: ${dn}")
			adn <- demoFeatures.avgDumNum
			_	<- ZIO.log(s"Average dumNum = ${adn}")
			sumTxt <- demoFeatures.summarizeStreamInSketch
			_ 	<- ZIO.log(s"Summarized stream as: ${sumTxt}")
		} yield ()
	}



}
trait ZStrmDemoFeatures {
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
	def avgDumNum : UIO[BigDecimal] = {
		val strm: ZStream[Any, Nothing, BigDecimal] = ZStream.repeatZIO(dumNum)
		val shortStrm = strm.take(500)
		val zeroPair : (Int, BigDecimal) = (0, BigDecimal("0.0"))
		// foldLeft[In, S](z: => S)(f: (S, In) => S)
		val accumSink = ZSink.foldLeft[BigDecimal, (Int, BigDecimal)](zeroPair)((prevStPair, nxtBD) => {
			(prevStPair._1 + 1, prevStPair._2.+(nxtBD))
		})
		val avgSink = accumSink.map(pair => {pair._2./(pair._1)})
		val shortAvg = shortStrm.run(avgSink)
		shortAvg
	}
	def summarizeStreamInSketch: UIO[String] = {
		val hsm = new HeavySktchMkr {}
		val emptyQSW = hsm.mkEmptyQSW_BD(16)
		val accumSink = ZSink.foldLeft[BigDecimal, QuantileSketchWriter[BigDecimal]](emptyQSW)((prevQSW, nxtBD) => {
			val nxtQSW = prevQSW.addItem(nxtBD)
			nxtQSW
		})
		val strm: ZStream[Any, Nothing, BigDecimal] = ZStream.repeatZIO(dumNum)
		val shortStrm = strm.take(500)
		val sketchUIO: UIO[QuantileSketchWriter[BigDecimal]] = shortStrm.run(accumSink)
		val summaryUIO: UIO[String] = sketchUIO.map(endQSW => {
			val qsr = endQSW.getSketchReader
			val summTxt = qsr.getSummaryTxt(true, true)
			val qArr: qsr.OutArrT = qsr.getQuantiles(12)
			val quantTxt = qArr.toList.toString
			val minV = qsr.getMinValue
			val maxV = qsr.getMaxValue
			val width = maxV.-(minV)
			val binCnt = 12
			val incr = width./(binCnt)
			val binIdxs = 1 to binCnt - 1
			val splits: Array[BigDecimal] = binIdxs.toSeq.map(idx => incr.*(idx).+(minV)).toArray
			val pmf: Array[Double] = qsr.getPMF(splits)
			val pmfSum = pmf.reduce(_ + _)
			val splitsTxt = splits.toList.toString()
			val pmfTxt = pmf.toList.toString()
			val statTxts = List(summTxt, "quants: " + quantTxt, "splits: " + splitsTxt, "pmf: " + pmfTxt, "pmfSum:" + pmfSum)
			statTxts.mkString("\n")
		})
		summaryUIO
	}
}
/*
https://zio.dev/reference/stream/

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