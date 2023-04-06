package fun.gravax.zaccum
import zio.stream.{Sink, UStream, ZSink, ZStream}
import zio.{Chunk, UIO, ZIO, ZIOAppDefault, Random => ZRandom}
import zio.{Console => ZConsole}
import java.io.IOException
private trait ZioAccumUtils


trait PureStreamAccumulator {
	type InRec
	type PartialResult
	type OutResult

	val flg_timeAndLogParts = true
	val flg_timeAndLogWhole = true

	// type USink[I, +L, +Z]= ZSink[Any, Nothing, I, L, Z]
	// RecSync needs no env-resources, cannot fail, and leaves no leftovers.
	type RecSink = ZSink[Any, Nothing, InRec, Nothing, PartialResult]
	type RsltSink = ZSink[Any, Nothing, PartialResult, Nothing, OutResult]
	val mySink : RecSink // ZSink[Any, Nothing, InRec, Nothing, PartialResult]
	// Passing a sink as argument reverses the variance arrows of the type-args to ZSink.
	def parallelPartialAccumRechunked(recSinkA : RecSink)(chnkSz : Int, numParFlows : Int, bufSz : Int)(inStrm : UStream[InRec]) : UStream[PartialResult] = {
		val chnkStrm: UStream[Chunk[InRec]] = inStrm.grouped(chnkSz)
		val chnkWithIdx: UStream[(Chunk[InRec], Long)] = chnkStrm.zipWithIndex
		// The type of chnkGrps is:  ZStream.GroupBy[Any, Nothing, Long, (Chunk[InRec], Long)]
		val chnkGrps = chnkWithIdx.groupByKey(pair => pair._2 % numParFlows, bufSz)
		val mergedPartialResultStrm: UStream[PartialResult] = chnkGrps.apply((key, strm) => {
			val localChnks: UStream[Chunk[InRec]] = strm.map(_._1)
			val localRecords: UStream[InRec] = localChnks.flattenChunks
			val accumZIO: UIO[PartialResult] = localRecords.run(recSinkA)
			val nxtZIO : UIO[PartialResult]  = if (flg_timeAndLogParts) {
				val timedAccumUIO = accumZIO.timed
				val loggedAccumUIO: UIO[PartialResult] = timedAccumUIO.flatMap(pair =>  {
					val (dur, partRslt) = pair
					val logUIO = ZIO.log(s"parAccumRechunked.partialResult key=${key} duration=${dur} result=${partRslt}")
					logUIO *> ZIO.succeed(partRslt)
				})
				loggedAccumUIO
			} else accumZIO
			ZStream.fromZIO(nxtZIO)
		})
		mergedPartialResultStrm
	}
	def accumOutResult(rsltSinkB : RsltSink)(partRsltStrm : UStream[PartialResult]) : UIO[OutResult] = {
		val outRsltUIO: UIO[OutResult] = partRsltStrm.run(rsltSinkB)
		val outUIO : UIO[OutResult]  = if (flg_timeAndLogWhole) {
			val timedUIO: UIO[(zio.Duration, OutResult)] = outRsltUIO.timed
			val loggedUIO = timedUIO.flatMap(pair =>  {
				val (dur, outRslt) = pair
				val logUIO = ZIO.log(s"accumOutResult(${dur}) result: ${outRslt}")
				logUIO *> ZIO.succeed(outRslt)
			})
			loggedUIO
		} else outRsltUIO
		outUIO
	}

	def parallelAccumRechunked(recSinkA : RecSink)(chnkSz : Int, numParFlows : Int, bufSz : Int)(rsltSinkB : RsltSink)(inStrm : UStream[InRec]) = {
		val partialResultStrm = parallelPartialAccumRechunked(recSinkA)(chnkSz, numParFlows, bufSz)(inStrm)
		val outRsltUIO = accumOutResult(rsltSinkB)(partialResultStrm)
		outRsltUIO
	}
	def hmm(recSinkA : RecSink) = {
		val h: RecSink => (Int, Int, Int) => UStream[InRec] => UStream[PartialResult] = (parallelPartialAccumRechunked _)
		val g: (Int, Int, Int) => UStream[InRec] => UStream[PartialResult] = h(recSinkA)
		val x: Int => Int => Int => UStream[InRec] => UStream[PartialResult] = g.curried
	}
	// contravariant type InRec occurs in invariant position in type zio.stream.ZSink[Any,Nothing,InRec,Nothing,PartialResult] of type RecSink
	// def eatSink(snk : RecSink) : Unit

}

/*
final class ZSink[-R, +E, -In, +L, +Z]
A ZSink[R, E, I, L, Z] is used to consume elements produced by a ZStream.
You can think of a sink as a function that will consume a variable amount of I elements (could be 0, 1, or many!),
might fail with an error of type E, and will eventually yield a value of type Z together with a remainder of type L as leftover.
 */