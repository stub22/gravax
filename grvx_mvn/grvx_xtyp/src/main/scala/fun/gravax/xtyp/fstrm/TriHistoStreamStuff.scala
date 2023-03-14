package fun.gravax.xtyp.fstrm

import cats.effect.IO
import fs2.Stream
import fun.gravax.xtyp.mathy.tridesc.TriShapeXactish
import spire.math.Algebraic

import java.math.RoundingMode
import scala.collection.immutable.TreeMap
import scala.math.ScalaNumber

private trait TriHistoStreamStuff

// TreeMap supports (from SortedMapOps):
// .maxBefore : Find the element with largest key less than a given key.
// .minAfter(key: K): Option[(K, V)]  :	Find the element with smallest key larger than or equal to a given key.
// 1) We want to use lowerBound as the key (seems more intuitive than upper bound).
// 2) Thus for a given incoming sample num, we want to search for matching bin using .maxBefore.
// 3) We define lowerBound as exclusive to match up with the strictly-less-than behav of .maxBefore
// TODO:  Consider supporting more number types.
case class NumRangeHistoBin(lowerBoundExcl : BigDecimal, upperBoundIncl : BigDecimal, count : Int) {

	def incr: NumRangeHistoBin = {
		val oldCnt = count
		copy(count = oldCnt + 1)
	}

	def combineWithMatchedBin(otherBin : NumRangeHistoBin) = {
		assert(otherBin.lowerBoundExcl == lowerBoundExcl)
		assert(otherBin.upperBoundIncl == upperBoundIncl)
		val sum = count + otherBin.count
		copy(count = sum)
	}

}
// Key is the lowerBoundExcl
case class NumRangeHisto(binMap : TreeMap[BigDecimal, NumRangeHistoBin]) {
	private def binFor(num : BigDecimal) = {
		binMap.maxBefore(num)
	}
	def addSample(num : BigDecimal) : NumRangeHisto = {
		val matchingBinPair = binFor(num).getOrElse {
			println(s"Failed bin-lookup for sample=${num} in binMap: ${binMap}")
			throw new Exception(s"Failed bin-lookup for sample=${num} in binMap with ${binMap.size} entries")
		}
		val (binKey, bin) = matchingBinPair
		val updatedBin = bin.incr
		val updatedBinMap = binMap.updated(binKey, updatedBin)
		val updatedHisto = NumRangeHisto(updatedBinMap)
		updatedHisto
	}
	// TODO:  Generalize the numbers we can add.
	def addSampleFromAlgNum(algNum : Algebraic) : NumRangeHisto = {
		val decimalPlaces = 8
		val sampleBD = algNum.toBigDecimal(decimalPlaces, RoundingMode.HALF_UP)
		addSample(sampleBD)
	}
	def totalSampleCount : Int = {
		binMap.foldLeft[Int](0)((prevSum, nxtPair) => {
			prevSum + nxtPair._2.count
		})
	}

	def combine(otherNRH : NumRangeHisto) : NumRangeHisto = {
		// If the bin boundaries match up perfectly, then the combination is easy.
		// That is the expected pattern in simplest use cases (where the binning was determined by the same source).
		// In principle we could also combine histograms that do not have perfectly matching bins, but the utility is
		// questionable.
		???
	}
	def combineIfBinsMatch(otherNRH : NumRangeHisto) : Option[NumRangeHisto] = {
		???
	}
	def combineAssumingBinsMatch(otherNRH : NumRangeHisto): NumRangeHisto = {
		val sumBinMap: TreeMap[BigDecimal, NumRangeHistoBin] = binMap.map(binPair => {
			val (binKey, binDat) = binPair
			val otherBinDat = otherNRH.binMap.getOrElse(binKey, {throw new Exception(s"Bins don't match at key: ${binKey}")})
			val summedBin = binDat.combineWithMatchedBin(otherBinDat)
			(binKey, summedBin)
		})
		new NumRangeHisto(sumBinMap)
	}
}

case class DiscreteHistoBin[BinKey](binKey : BinKey, count : Int) {
	def incr: DiscreteHistoBin[BinKey] = {
		val oldCnt = count
		copy(count = oldCnt + 1)
	}

	def combineWithMatchedBin(otherBin : DiscreteHistoBin[BinKey]) : DiscreteHistoBin[BinKey] = {
		assert(otherBin.binKey == binKey)
		val sum = count + otherBin.count
		copy(count = sum)
	}


}
// TreeMap uses an implicit Ordering[BinKey].
// This histo *can* automatically expand to contain bins for new keys.
case class DiscreteHisto[BinKey : Ordering](binMap : TreeMap[BinKey, DiscreteHistoBin[BinKey]]) {
	private def binFor(bk : BinKey) = {
		binMap.get(bk)
	}
	def addSample(bk : BinKey) : DiscreteHisto[BinKey] = {
		val nextMap = binMap.updatedWith(bk)(prevBinOpt => Some(prevBinOpt.fold(
			new DiscreteHistoBin[BinKey](bk, 1)) // None case
		(prevBin => prevBin.incr))) // Some case
		copy(binMap = nextMap)
	}

	def combine(otherH : DiscreteHisto[BinKey]) : DiscreteHisto[BinKey] = {
		// TODO:  Do this in a more monoid-combiney way.
		// https://stackoverflow.com/questions/20047080/scala-merge-map
		val myPairs = binMap.toSeq
		val otherPairs = otherH.binMap.toSeq
		val combinedPairs: Seq[(BinKey, DiscreteHistoBin[BinKey])] = myPairs ++ otherPairs
		val summed: Map[BinKey, DiscreteHistoBin[BinKey]] = combinedPairs.groupMapReduce(_._1)(_._2)(_.combineWithMatchedBin(_))
		val comboTreeMap  = TreeMap.from[BinKey, DiscreteHistoBin[BinKey]](summed)
		DiscreteHisto(comboTreeMap)
	}
	def totalSampleCount : Int = {
		binMap.foldLeft[Int](0)((prevSum, nxtPair) => {
			prevSum + nxtPair._2.count
		})
	}
}

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
		val emptyHisto = mkEmptyHisto
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
		val emptyResultPair : (FailureCount, NumRangeHisto) = (0, mkEmptyHisto)
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

		val emptyHisto = mkEmptyHisto

		val streamOfPartialHistoRslts: Stream[IO, NumRangeHisto] = winStrm.scan(emptyHisto)((prevHisto, nxtTri) => {
			val nxtAreaAlg: Algebraic = nxtTri.area
			prevHisto.addSampleFromAlgNum(nxtAreaAlg)
		})
		streamOfPartialHistoRslts
	}
	def partialResults(strm: Stream[IO, OurTriGenRslt]) : Stream[IO, NumRangeHisto] = ???

	def mkRegBounds : Seq[BigDecimal] = {
		val min = BigDecimal("0.0")
		val regMax = BigDecimal("100.0")
		val step = BigDecimal("10.0")
		// Boundaries of all the regular bins.
		// Usually we have an additional catch-all bin at the lower and upper extremities.
		// We may also wish to have an error bin.
		val regBounds: Seq[BigDecimal] = min to regMax by step
		regBounds
	}
	def mkEmptyBinSeqForRegBounds (regBounds: Seq[BigDecimal]): Seq[NumRangeHistoBin]  = {
		val ultMin = BigDecimal(Long.MinValue)
		val ultMax = BigDecimal(Long.MaxValue)
		val lowerBounds = ultMin +: regBounds
		val upperBounds = regBounds :+ ultMax
		val boundPairs = lowerBounds.zip(upperBounds)
		val bins = boundPairs.map(pair => NumRangeHistoBin(pair._1, pair._2, 0))
		bins
	}
	def mkEmptyHisto : NumRangeHisto = {
		val regBounds = mkRegBounds
		val binSeq = mkEmptyBinSeqForRegBounds(regBounds)
		val keyBinPairs: Seq[(BigDecimal, NumRangeHistoBin)] = binSeq.map(bin => (bin.lowerBoundExcl, bin))
		val tm = TreeMap[BigDecimal, NumRangeHistoBin](keyBinPairs:_*)
		NumRangeHisto(tm)
	}


}
