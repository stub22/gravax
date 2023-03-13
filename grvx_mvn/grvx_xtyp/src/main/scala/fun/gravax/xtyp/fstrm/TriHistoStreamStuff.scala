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
	def oldCnt = count
	def incr = copy(count = oldCnt + 1)

	def combineWithMatchedBin(otherBin : NumRangeHistoBin) = {
		assert(otherBin.lowerBoundExcl == lowerBoundExcl)
		assert(otherBin.upperBoundIncl == upperBoundIncl)
		val sum = count + otherBin.count
		copy(count = sum)
	}

}
// Key is the lowerBoundExcl
case class NumRangeHisto(binMap : TreeMap[BigDecimal, NumRangeHistoBin]) {
	def binFor(num : BigDecimal) = {
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

trait TriHistoStreamMaker {
	val myPipeOps = new TriStrmPipeOps{}
	val myUtilStrms = new SomeUtilityStreams {}
	type OurTriErrMsg = String
	type OurTriGenRslt = Either[OurTriErrMsg, TriShapeXactish]

	def foldToOneHistoResult(strm: Stream[IO, OurTriGenRslt]) : Stream[IO, NumRangeHisto] = {
		val winStrm: Stream[IO, TriShapeXactish] = strm.through(myPipeOps.onlyWins)
		// val parWinStrm = winStrm.parEvalMap()
		val emptyHisto = mkEmptyHisto
		val streamOfOneHistoRslt: Stream[IO, NumRangeHisto] = winStrm.fold(emptyHisto)((prevHisto, nxtTri) => {
			val nxtAreaAlg: Algebraic = nxtTri.area
			prevHisto.addSampleFromAlgNum(nxtAreaAlg)
		})
		streamOfOneHistoRslt
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
