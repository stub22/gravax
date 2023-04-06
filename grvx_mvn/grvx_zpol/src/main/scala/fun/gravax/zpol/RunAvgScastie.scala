package fun.gravax.zpol

import zio.stream.{Sink, UStream, ZSink, ZStream}
import zio.{Chunk, UIO, ZIO, ZIOAppDefault, Random => ZRandom}
import zio.{Console => ZConsole}
import java.io.IOException
import java.math.{MathContext, RoundingMode}

// Under Scastie we remove the      extends ZIOAppDefault {
object RunAveragingLikeScastie extends ZIOAppDefault {

	def run = myAppLogic

	val myAppLogic: ZIO[Any, IOException, Unit] = {
		val demoFeat = new AvgFun {}
		val sizeToCnt = 1 * 256
		// We use both zio.Console and ZIO.log.
		for {
			_	<- ZConsole.printLine("zio.Console.printLine says RunAveragingDemo BEGIN")
			_ <- ZIO.log("ZIO.log says RunAveragingDemo BEGIN")
			adn <- demoFeat.avgGumNums(sizeToCnt)
			_	<- ZIO.log(s"Average gumNum = ${adn}")
			_	<- ZConsole.printLine("ZConsole.printLine:  In Scastie we do NOT invoke parAvgUsingChunks(itemCnt, chnkSize, numGroups, bufSz) : UIO[BigDecimal]\nSee source code")
		} yield ()
	}
}
// AvgFun shows a single-threaded calculation that we can run here in Scastie.
// ParAvgFun below shows a parallel version of the algorithm.
trait AvgFun {
	// This trait computes an average of a ZStream of gaussian-distributed BigDecimals.
	// Averaging calc is defined using ZSinks, which are applied to the random num stream by the .avgGumNums method.


	// USink cannot fail, needs no environment.  L=Leftover is also unused [Nothing] below.

	type USink[A, +L, +B]= ZSink[Any, Nothing, A, L, B]

	val myZeroAccumPair : (Int, BigDecimal) = (0, BigDecimal("0.0"))

	// Innermost sink used to count_and_sum the input numbers. Type sig:  foldLeft[In, S](z: => S)(f: (S, In) => S)
	val myAccumSink: USink[BigDecimal, Nothing, (Int, BigDecimal)] =
		ZSink.foldLeft[BigDecimal, (Int, BigDecimal)](myZeroAccumPair)((prevAccumPair, nxtBD) =>
			(prevAccumPair._1 + 1, prevAccumPair._2.+(nxtBD)))

	// Outer avgSync absorbs values from accumSync and finalizes an averaging calculation, for the simple single-threaded case we run.
	val myAvgSink: USink[BigDecimal, Nothing, BigDecimal] = myAccumSink.map(pair => {pair._2./(pair._1)})

	// Given infra + distribution params, make an effect to generate one gaussian num.  zrnd = a pre-existing rng-ctx
	def gaussianBD(zrnd : ZRandom, mathCtx : MathContext) (mean : BigDecimal, stdDev : BigDecimal): UIO[BigDecimal] = {
		val stdNumEff: UIO[Double] = zrnd.nextGaussian
		stdNumEff.map(num => {
			// https://blogs.oracle.com/javamagazine/post/four-common-pitfalls-of-the-bigdecimal-class-and-how-to-avoid-them
			val stdBD = BigDecimal(num, mathCtx)
			val scaledAndShiftedBD = stdBD.*(stdDev).+(mean)
			scaledAndShiftedBD
		})
	}
	def gumNum : UIO[BigDecimal] = {
		val mean = BigDecimal("-5.0")
		val dev = BigDecimal("4.5")
		val mathCtx = new MathContext(8, RoundingMode.HALF_UP)
		// ZRandom.RandomLive is a presumed-existing RNG context, a ZIO wrapper for a Scala.Random.
		// To make the code deterministic, use your own seed to create your own ZRandom, instead.
		gaussianBD(ZRandom.RandomLive, mathCtx)(mean, dev)
	}

	// Build an effect that generates a stream of cnt random numbers, averages them, clock-times how long it took to do it, logs result.
	def avgGumNums(cnt : Int) : UIO[BigDecimal] = {
		val infiniteStrm: UStream[BigDecimal] = ZStream.repeatZIO(gumNum)
		val finiteStrm = infiniteStrm.take(cnt)
		val avgUIO: UIO[BigDecimal] = finiteStrm.run(myAvgSink)
		val timedAvgUIO: UIO[(zio.Duration, BigDecimal)] = avgUIO.timed
		val loggedAvgUIO = timedAvgUIO.flatMap(pair =>  {
			val logUIO = ZIO.log(s"avgGumNums(${cnt}) result: ${pair}")
			logUIO *> ZIO.succeed(pair._2)
		})
		loggedAvgUIO
	}
}

// Parallel impl is not invoked here in Scastie, but we can see it compile.
trait ParAvgFun extends AvgFun {
	// Separate sink chain is used with parallel chunk-groups impl below (not run in Scastie).
	// This sink combines pairs holding (count : Int, sum : BigDecimal), by summing each field separately.
	val myAccumCombSink = ZSink.foldLeft[(Int, BigDecimal), (Int, BigDecimal)](myZeroAccumPair)((prevAccumPair, nxtAccumPair) => {
		(prevAccumPair._1 + nxtAccumPair._1, prevAccumPair._2.+(nxtAccumPair._2))
	})
	// Final-stage sink produces the output average:
	val myCombAvgSink = myAccumCombSink.map(pair => {pair._2./(pair._1)})


	def parAvgUsingChunks(itemCnt : Int, chnkSize : Int, numGroups : Int, bufSz : Int) : UIO[BigDecimal] = {
		val origStrm: UStream[BigDecimal] = ZStream.repeatZIO(gumNum)
		val finiteStrm = origStrm.take(itemCnt)
		val chnkStrm: UStream[Chunk[BigDecimal]] = finiteStrm.grouped(chnkSize)
		val chnkWithIdx: UStream[(Chunk[BigDecimal], Long)] = chnkStrm.zipWithIndex
		val chnkGrps: ZStream.GroupBy[Any, Nothing, Long, (Chunk[BigDecimal], Long)] = chnkWithIdx.groupByKey(pair => pair._2 % numGroups, bufSz)
		val mergedResultStrm: UStream[(Int, BigDecimal)] = chnkGrps.apply((key, strm) => {
			val localChnks: UStream[Chunk[BigDecimal]] = strm.map(_._1)
			val localNums: UStream[BigDecimal] = localChnks.flattenChunks
			// Count-and-sum the input numbers.
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
/***
// Runs app in Scastie. uncommented
zio.Unsafe.unsafe(implicit us =>
scala.concurrent.Await.result(
zio.Runtime.default.unsafe.runToFuture(RunAveragingDemo.run),
scala.concurrent.duration.Duration.Inf
)
)
*/