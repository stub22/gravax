package fun.gravax.xtyp.fstrm

import cats.effect.IO
import fs2.Stream
import fun.gravax.xtyp.histo.{DiscreteHisto, DiscreteHistoBin, NumRangeHisto, NumRangeHistoHelper}
import fun.gravax.xtyp.mathy.tridesc.TriShapeXactish
import spire.math.Algebraic

import java.math.RoundingMode
import scala.collection.immutable.TreeMap
import scala.math.ScalaNumber

private trait TriHistoStreamStuff


// Treat each kind of failure as a String key for a DiscreteHistoBin.
case class HistoForFailureOrNumRange(failH : DiscreteHisto[String], numH : NumRangeHisto) {
	def combine(otherH : HistoForFailureOrNumRange) : HistoForFailureOrNumRange = {
		val combFailH = failH.combine(otherH.failH)
		val combNumH = numH.combineAssumingBinsMatch(otherH.numH)
		HistoForFailureOrNumRange(combFailH, combNumH)
	}
	def totalSampleCount : Int = failH.totalSampleCount + numH.totalSampleCount

	def addSample(samp : Either[String, Algebraic]) : HistoForFailureOrNumRange  = {
		val (nxtFailH, nxtNumH) = samp match {
			case Left(failStrng : String) => (failH.addSample(failStrng), numH)
			case Right(algNum : Algebraic) => (failH, numH.addSampleFromAlgNum(algNum))
		}
		HistoForFailureOrNumRange(nxtFailH, nxtNumH)
	}
}

trait TriHistoStreamMaker {
	val myNRHH = new NumRangeHistoHelper{}
	val myPipeOps = new TriStrmPipeOps{}
	val myUtilStrms = new SomeUtilityStreams {}
	type OurTriErrMsg = String
	type OurTriGenRslt = Either[OurTriErrMsg, TriShapeXactish]

	def foldToOneHistoResult(strm: Stream[IO, OurTriGenRslt]) : Stream[IO, NumRangeHisto] = {
		// FIXME:  We would like to be able to be able to count the number of failures (illegal triangles).
		// The problem is cleanly capturing that result into our output.
		// Option 1)  We could create a histogram bin that corresponds to failures.  However note that it
		// would need both a key and value in the bin map, so we will be doing some significant design to
		// capture the failure info.
		// Option 2)  Errors get counted and printed by a second pipe, which ultimately outputs Nothing.
		// (If we don't suppress the output, then we will wind up with a messy combined output stream of type Stream[IO, Any])
		// Option 3)  Insert another layer of aggregate results, equivalent to Either[FailureCount, NumRangeHisto].


		val flg_dumpFailureCnt = true
		val winStrm: Stream[IO, TriShapeXactish] = if (flg_dumpFailureCnt) {
			strm.broadcastThrough(myPipeOps.countAndPrintTriFailuresButEmitNothing, myPipeOps.onlyWins)
		} else strm.through(myPipeOps.onlyWins)
		// val parWinStrm = winStrm.parEvalMap()
		val emptyHisto = myNRHH.mkEmptyHisto
		val streamOfOneHistoRslt: Stream[IO, NumRangeHisto] = winStrm.fold(emptyHisto)((prevHisto, nxtTri) => {
			val nxtAreaAlg: Algebraic = nxtTri.area
			prevHisto.addSampleFromAlgNum(nxtAreaAlg)
		})
		streamOfOneHistoRslt
	}

	// Here is the an implementation that accumulates the failures into a separate FailureCount value.
	// This provides us with an output pair:  (FailureCount, NumRangeHisto), which makes sense, but...
	type FailureCount = Int
	def foldToOneComboResult(strm: Stream[IO, OurTriGenRslt]) : Stream[IO, (FailureCount, NumRangeHisto)] = {
		val emptyResultPair : (FailureCount, NumRangeHisto) = (0, myNRHH.mkEmptyHisto)
		strm.fold[(FailureCount, NumRangeHisto)] (emptyResultPair)((prevPair, nxtTriGenRslt) => {
			val (prevFailCnt, prevHisto) = prevPair
			nxtTriGenRslt.fold(errMsg => (prevFailCnt + 1, prevHisto), goodTri => {
				val nxtAreaAlg: Algebraic = goodTri.area
				val nxtHisto = prevHisto.addSampleFromAlgNum(nxtAreaAlg)
				(prevFailCnt, nxtHisto)
			})
		})
	}

	def foldToOneTotalResult(strm: Stream[IO, OurTriGenRslt]) : Stream[IO, HistoForFailureOrNumRange] = {
		val comboRslt = foldToOneComboResult(strm)
		comboRslt.map(rsltPair => {
			val (failCnt, numH) = rsltPair
			val failKey = "FAILED"
			// FIXME:  These steps should be going through some kind of CreateHisto API, or Monoid.unit.
			val failBin = new DiscreteHistoBin[String](failKey, failCnt)
			val failMap = TreeMap[String, DiscreteHistoBin[String]](failKey -> failBin)
			val failH = DiscreteHisto[String](failMap)
			HistoForFailureOrNumRange(failH, numH)
		})
	}

	def scanToPartialHistoResults(strm: Stream[IO, OurTriGenRslt]) : Stream[IO, NumRangeHisto] = {
		val winStrm: Stream[IO, TriShapeXactish] = strm.through(myPipeOps.onlyWins)

		val emptyHisto = myNRHH.mkEmptyHisto

		val streamOfPartialHistoRslts: Stream[IO, NumRangeHisto] = winStrm.scan(emptyHisto)((prevHisto, nxtTri) => {
			val nxtAreaAlg: Algebraic = nxtTri.area
			prevHisto.addSampleFromAlgNum(nxtAreaAlg)
		})
		streamOfPartialHistoRslts
	}
	def partialResults(strm: Stream[IO, OurTriGenRslt]) : Stream[IO, NumRangeHisto] = ???



}
