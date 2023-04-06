package fun.gravax.zpol

import zio.stream.{Sink, UStream, ZSink, ZStream}
import zio.{Chunk, UIO, ZIO, ZIOAppDefault, Random => ZRandom}

import java.io.IOException
import java.math.{MathContext, RoundingMode}


object RunAveragingDemo extends ZIOAppDefault {

	def run = myAppLogic

	val myAppLogic: ZIO[Any, IOException, Unit] = {
		val demoFeatures = new AveragingDemoFeatures {}
		val sizeToCnt = 1 * 1000 * 1000
		val parGroups = 8
		val parBufSz = 16 * 16 * 16
		for {
			_	<- zio.Console.printLine("zio.Console.printLine says Heya")
			_	<- ZIO.log("RunAveragingDemo BEGIN")
			zs  <- demoFeatures.sumPositiveInts(1000)
			_	<- ZIO.log(s"Sum result: ${zs}")
			dn <- demoFeatures.dumNum
			_	<- ZIO.log(s"Dumb rando: ${dn}")
			ddn <- demoFeatures.drainDumNums(sizeToCnt)
			_	<- ZIO.log(s"Drained dumNums = ${ddn}")
			ddn2 <- demoFeatures.drainDumNums(sizeToCnt)
			_	<- ZIO.log(s"Drained AGAIN dumNums = ${ddn2}")
			cdn <- demoFeatures.countDumNums(sizeToCnt)
			_	<- ZIO.log(s"Counted dumNums = ${cdn}")
			pcdn <- demoFeatures.parCountDumNums(sizeToCnt, parGroups, parBufSz)
			_	<- ZIO.log(s"parCounted dumNums = ${pcdn}")
			adn <- demoFeatures.avgDumNums(sizeToCnt)
			_	<- ZIO.log(s"Average dumNum = ${adn}")
			tdn <- demoFeatures.thickerAvg(sizeToCnt)
			_	<- ZIO.log(s"Thicker-Average dumNum = ${tdn}")
			padn <- demoFeatures.parAvgDumNum(sizeToCnt, parGroups, parBufSz )
			_	<- ZIO.log(s"Par-Average dumNum = ${padn}")
			pauc <- demoFeatures.parAvgUsingChunks(sizeToCnt, 1024, parGroups, 16)
			_	<- ZIO.log(s"Par-Average-Using-Chunks dumNum = ${pauc}")
		} yield ()
	}



}
trait AveragingDemoFeatures {
	def sumPositiveInts(posInt : Int) : UIO[Int] = {
		val rangeToSum = 1 to posInt
		val stream: ZStream[Any, Nothing, Int] = ZStream.fromIterable(rangeToSum)
		val sink: ZSink[Any, Nothing, Int, Nothing, 	Int] = ZSink.sum[Int]
		val sum: ZIO[Any, Nothing, Int] = stream.run(sink)
		val sumUIO : UIO[Int] = sum
		sumUIO
	}

	def gaussianBD(zrnd : ZRandom, mathCtx : MathContext)(mean : BigDecimal, stdDev : BigDecimal): UIO[BigDecimal] = {
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
		gaussianBD(ZRandom.RandomLive, mathCtx)(mean, dev)
	}
	type USink[A, +L, +B]= ZSink[Any, Nothing, A, L, B]
	val myZeroAccumPair : (Int, BigDecimal) = (0, BigDecimal("0.0"))
	val myAccumSink: USink[BigDecimal, Nothing, (Int, BigDecimal)] = // foldLeft[In, S](z: => S)(f: (S, In) => S)
		ZSink.foldLeft[BigDecimal, (Int, BigDecimal)](myZeroAccumPair)((prevAccumPair, nxtBD) =>
			(prevAccumPair._1 + 1, prevAccumPair._2.+(nxtBD)))
	val myAvgSink: USink[BigDecimal, Nothing, BigDecimal] = myAccumSink.map(pair => {pair._2./(pair._1)})

	val myAccumCombSink = ZSink.foldLeft[(Int, BigDecimal), (Int, BigDecimal)](myZeroAccumPair)((prevStPair, nxtAccumPair) => {
		(prevStPair._1 + nxtAccumPair._1, prevStPair._2.+(nxtAccumPair._2))
	})

	val myCombAvgSink = myAccumCombSink.map(pair => {pair._2./(pair._1)})

	// Notice the difference in type signature in the 4th position.  Here L/Leftover = BigDecimal, while above is Nothing.
	val mySlowerAccumSink: ZSink[Any, Nothing, BigDecimal, BigDecimal, (Int, BigDecimal)] = {
		val sleepDur = zio.Duration.fromNanos(1)
		ZSink.foldLeftZIO[Any, Nothing, BigDecimal, (Int, BigDecimal)](myZeroAccumPair)((prevStPair, nxtBD) => {
			// Doing any nonzero amount of sleep seems to take > 15 microsec
			ZIO.sleep(sleepDur) *>
			ZIO.succeed(prevStPair._1 + 1, prevStPair._2.+(nxtBD))
		})
	}

	val mySlowerAvgSink = mySlowerAccumSink.map(pair => {pair._2./(pair._1)})

	def drainDumNums(cnt : Int) : UIO[Unit] = {
		val infiniteStrm: UStream[BigDecimal] = ZStream.repeatZIO(dumNum)
		val finiteStrm = infiniteStrm.take(cnt)
		val drainyUIO: UIO[Unit] = finiteStrm.runDrain
		val timedCntUIO: UIO[(zio.Duration, Unit)] = drainyUIO.timed
		val loggedAvgUIO = timedCntUIO.flatMap(pair =>  {
			val logUIO = ZIO.log(s"drainDumNums(${cnt}) result: ${pair}")
			logUIO *> ZIO.succeed(pair._2)
		})
		loggedAvgUIO
	}

	def countDumNums(cnt : Int) : UIO[Long] = {
		val infiniteStrm: UStream[BigDecimal] = ZStream.repeatZIO(dumNum)
		val finiteStrm = infiniteStrm.take(cnt)
		val countUIO: UIO[Long] = finiteStrm.runCount
		val timedCntUIO: UIO[(zio.Duration, Long)] = countUIO.timed
		val loggedAvgUIO = timedCntUIO.flatMap(pair =>  {
			val logUIO = ZIO.log(s"countDumNums(${cnt}) result: ${pair}")
			logUIO *> ZIO.succeed(pair._2)
		})
		loggedAvgUIO
	}
	def parCountDumNums(cnt : Int, numGroups : Int, bufSz : Int) : UIO[Long] = {
		val infiniteStrm: UStream[BigDecimal] = ZStream.repeatZIO(dumNum)
		val strmWithIdx: UStream[(BigDecimal, Long)] = infiniteStrm.zipWithIndex
		val finiteStrm = strmWithIdx.take(cnt)
		val grouped = finiteStrm.groupByKey(pair => pair._2 % numGroups, bufSz)
		val mergedResultStrm: UStream[Long] = grouped.apply((key, strm) => ZStream.fromZIO(strm.runCount))
		val mergedCountUIO: UIO[Long] = mergedResultStrm.runSum
		val timedCntUIO: UIO[(zio.Duration, Long)] = mergedCountUIO.timed
		val loggedAvgUIO = timedCntUIO.flatMap(pair =>  {
			val logUIO = ZIO.log(s"parCountDumNums(${cnt}, ${numGroups}, ${bufSz}) result: ${pair}")
			logUIO *> ZIO.succeed(pair._2)
		})
		loggedAvgUIO
	}
	def avgDumNums(cnt : Int) : UIO[BigDecimal] = {
		val infiniteStrm: UStream[BigDecimal] = ZStream.repeatZIO(dumNum)
		val finiteStrm = infiniteStrm.take(cnt)
		val avgUIO: UIO[BigDecimal] = finiteStrm.run(myAvgSink)
		val timedAvgUIO: UIO[(zio.Duration, BigDecimal)] = avgUIO.timed
		val loggedAvgUIO = timedAvgUIO.flatMap(pair =>  {
			val logUIO = ZIO.log(s"avgDumNums(${cnt}) result: ${pair}")
			logUIO *> ZIO.succeed(pair._2)
		})
		loggedAvgUIO
	}
	def thickerAvg(cnt : Int) : UIO[BigDecimal] = {
		val infiniteStrm: UStream[BigDecimal] = ZStream.repeatZIO(dumNum)
		val strmWithIdx: UStream[(BigDecimal, Long)] = infiniteStrm.zipWithIndex
		val finiteStrm = strmWithIdx.take(cnt).map(_._1)
		val avgUIO: UIO[BigDecimal] = finiteStrm.run(myAvgSink)
		val timedAvgUIO: UIO[(zio.Duration, BigDecimal)] = avgUIO.timed
		val loggedAvgUIO = timedAvgUIO.flatMap(pair =>  {
			val logUIO = ZIO.log(s"thickerAvg(${cnt}) result: ${pair}")
			logUIO *> ZIO.succeed(pair._2)
		})
		loggedAvgUIO
	}
	def parAvgDumNum(cnt : Int, numGroups : Int, bufSz : Int) : UIO[BigDecimal] = {
		val origStrm: UStream[BigDecimal] = ZStream.repeatZIO(dumNum)
		// Tag each record with a sequence number that we can use later in .groupBy.
		val strmWithIdx: UStream[(BigDecimal, Long)] = origStrm.zipWithIndex
		val finiteStrm = strmWithIdx.take(cnt)
		// grouped : ZStream.GroupBy[Any, Nothing, Long, (BigDecimal, Long)]
		val grouped = finiteStrm.groupByKey(pair => pair._2 % numGroups, bufSz)
		val mergedResultStrm: UStream[(Int, BigDecimal)] = grouped.apply((key, strm) => {
			val accumZIO: UIO[(Int, BigDecimal)] = strm.map(_._1).run(myAccumSink)
			val timedAccumUIO = accumZIO.timed
			val loggedAccumUIO: UIO[(Int, BigDecimal)] = timedAccumUIO.flatMap(pair =>  {
				val logUIO = ZIO.log(s"parAvg.local key=${key} result: ${pair}")
				logUIO *> ZIO.succeed(pair._2)
			})
			val azs: UStream[(Int, BigDecimal)] = ZStream.fromZIO(loggedAccumUIO)
			azs
		})
		val dbgMRS = mergedResultStrm.debug("parAvgDumNum: partial result")
		val combAvgUIO: UIO[BigDecimal] = dbgMRS.run(myCombAvgSink)
		val timedAvgUIO: UIO[(zio.Duration, BigDecimal)] = combAvgUIO.timed
		val loggedAvgUIO = timedAvgUIO.flatMap(pair =>  {
			val logUIO = ZIO.log(s"parAvgDumNum(${cnt}, ${numGroups}, ${bufSz}) result: ${pair}")
			logUIO *> ZIO.succeed(pair._2)
		})
		loggedAvgUIO
	}
	def parAvgUsingChunks(itemCnt : Int, chnkSize : Int, numGroups : Int, bufSz : Int) : UIO[BigDecimal] = {
		val origStrm: UStream[BigDecimal] = ZStream.repeatZIO(dumNum)
		val finiteStrm = origStrm.take(itemCnt)
		val chnkStrm: UStream[Chunk[BigDecimal]] = finiteStrm.grouped(chnkSize)
		val chnkWithIdx: UStream[(Chunk[BigDecimal], Long)] = chnkStrm.zipWithIndex
		val chnkGrps = chnkWithIdx.groupByKey(pair => pair._2 % numGroups, bufSz)
		val mergedResultStrm = chnkGrps.apply((key, strm) => {
			val localChnks: UStream[Chunk[BigDecimal]] = strm.map(_._1)
			val localNums: UStream[BigDecimal] = localChnks.flattenChunks
			val accumZIO: UIO[(Int, BigDecimal)] = localNums.run(myAccumSink)
			val timedAccumUIO = accumZIO.timed
			val loggedAccumUIO: UIO[(Int, BigDecimal)] = timedAccumUIO.flatMap(pair =>  {
				val logUIO = ZIO.log(s"parAvgUsingChunks.local parAccumRechunked key=${key}  result: ${pair}")
				logUIO *> ZIO.succeed(pair._2)
			})
			val azs: UStream[(Int, BigDecimal)] = ZStream.fromZIO(loggedAccumUIO)
			azs
		})
		val dbgMRS = mergedResultStrm.debug("parAvgUsingChunks: partial result")
		val combAvgUIO: UIO[BigDecimal] = dbgMRS.run(myCombAvgSink)
		val timedAvgUIO: UIO[(zio.Duration, BigDecimal)] = combAvgUIO.timed
		val loggedAvgUIO = timedAvgUIO.flatMap(pair =>  {
			val logUIO = ZIO.log(s"parAvgUsingChunks(itemCnt=${itemCnt}, chnkSize=${chnkSize}, ${numGroups}, ${bufSz}) result: ${pair}")
			logUIO *> ZIO.succeed(pair._2)
		})
		loggedAvgUIO
	}

}
/*
Most expensive step is RNG.
Next is the distribute-to-queues stuff going on inside of .groupByKey.

def rechunk(n: => Int)(implicit trace: Trace): ZStream[R, E, A]
Re-chunks the elements of the stream into chunks of n elements each.

def grouped(chunkSize: => Int)(implicit trace: Trace): ZStream[R, E, Chunk[A]]
Partitions the stream with specified chunkSize

def groupAdjacentBy[K](f: (A) => K)(implicit trace: Trace): ZStream[R, E, (K, NonEmptyChunk[A])]
Creates a pipeline that groups on adjacent keys, calculated by function f.

def transduce[R1 <: R, E1 >: E, A1 >: A, Z](sink: => ZSink[R1, E1, A1, A1, Z])(implicit trace: Trace): ZStream[R1, E1, Z]
Applies the transducer to the stream and emits its outputs.


 */

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