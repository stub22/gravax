package fun.gravax.dbin.model

import fun.gravax.dbin.model.DbinModelTypes.{BinRelWt, BinTag, EntryKey}
import zio.{Task, UIO, ZIO}
import zio.stream.{UStream, ZStream}

private trait DbinQueryStuff

// Client wants to give us operators to be applied and combined over the bin trees
// Apply entryMean over entries [a, b, c] of binTree27 at sublevel 3, to yield [a-mean, b-mean, c-mean]
// Apply entryCoVar over entries [d, e, f] of binTree21 at sublevel 2, to yield [dd-var, de-cov, df-cov, ee-var, ef-cov, ff-var]
// This requires iterating over the sub-bins and computing a partial result from each one, then combining those
// results.  It's a map-reduce style computation.
// But one complication is that we need to weight the sub-results properly, depending on what we consider to be
// the root-bin of the computation.
// a) Intermediate result type to be combined
// Apply entryMedian over ...

/**
 * Refinement of bins is somewhat like clustering.
 * We
 */

trait  BinQueryOps {
}
object BinQueryTypes {
	type Result
	type BinApply
	type ResultCombine
}

/*
We apply Transforms to bins to define new entries, which can be functions of existing entries.
The result is a new bin.

We apply map+reduce operations to bins to calculate statistics over the entries.
The result is a tensor of numbers.
*/
trait BinQueryCtx extends EntryStatTypes {
	/*
Opaque type aliases cannot be private and cannot be overridden in subclasses.
Opaque type aliases cannot have a context function type as right-hand side.
	 */
	// "opaque type must have a right hand side"
	type BinHandle // How we hand a bin to an operation


	/** *
	 * Assignment by a parent bin (or chain of ancestors) of the relative probability weight of a child bin.
	 * Normalization of the weight-scale is context sensitive.  A bin has weight relative to its peers,
	 * in the context of its ancestors, who may be various.
	 */
	type BinRelWeight = DbinModelTypes.BinRelWt


	// type PartialResult  // What the operation hands back
	// type PRWtPair = (PartialResult, BinWeight)


	// Defines a mapReduce operation in terms of required constituent operations
	trait BinOperation[Rslt] {
		// Asks a bin for some of its entries, and does some calculation with them.
		// A bin does not know its own weight!
		// The Rslt produced for the binHandle is our best estimate of the requested quantity from the persp. of that bin.
		// From the bin's own internal perspective, it must have total weight 1.0
		def getBinIntrinsicResult(binHandle: BinHandle): Rslt

		def zeroResult: Rslt

		type WeightedRslt = Rslt // A Rslt which has been mulitplied by BinRelWt

		// It is useful (as a checksum, +?) to sum up the bin weights as well as the weighted results.
		type WeightedResultPair = (BinRelWeight, WeightedRslt)

		case class WeightedWrapper(intrinsicResult: Rslt, binRelWt: BinRelWeight, weightedRslt: WeightedRslt)

		private val zeroWt: BinRelWeight = ???
		private val zeroWR = (zeroWt, zeroResult)

		def makeWeightedWrapper: WeightedWrapper

		protected def mkWeightedResult(binRelWt: BinRelWt, unwtRslt: Rslt): WeightedRslt

		def mkWtRsltPair(binRelWt: BinRelWt, unwtRslt: Rslt): WeightedResultPair = (binRelWt, mkWeightedResult(binRelWt, unwtRslt))

		// Does a monoidal combine
		def wrCombine(wr1: WeightedResultPair, wr2: WeightedResultPair): WeightedResultPair

		def combineResults(prevAccum: Rslt, nextWt: BinRelWeight, nxtPartial: Rslt): Rslt
	}


	trait BinQueryApi[F[_]] {
		def getBinTag(bin: BinHandle): BinTag

		def getParentTag(bin: BinHandle): Option[BinTag]

		// Some entries may need calculation, probably via some binOperation
		def getEntries(bin: BinHandle)(keys: Set[EntryKey]): F[Map[EntryKey, BinEntry]]

		def applyBinOperation[Rslt](binOp: BinOperation[Rslt])(bin: BinHandle, subLevelNum: Int): F[Rslt]

		def applyBinTransform(binXform: BinTransformer)(bin: BinHandle): BinHandle = binXform.transform(bin)

	}

	trait BinDataZStreamer {
		def streamDescendantLevel(bin: BinHandle, subLevelNum: Int): UStream[(BinHandle, BinRelWeight)]
	}

	class BQZApi(bdz: BinDataZStreamer) extends BinQueryApi[Task] {
		override def getBinTag(bin: BinHandle): BinTag = ???

		override def getParentTag(bin: BinHandle): Option[BinTag] = ???

		override def getEntries(bin: BinHandle)(keys: Set[EntryKey]): Task[Map[EntryKey, BinEntry]] = {
			???
		}

		override def applyBinOperation[Rslt](binOp: BinOperation[Rslt])(bin: BinHandle, subLevelNum: Int): Task[Rslt] = {
			val subBinWtPairs = bdz.streamDescendantLevel(bin, subLevelNum)
			val subResultWtPairs: UStream[(Rslt, BinRelWeight)] = subBinWtPairs.map(binWtPair => {
				val (bin, wt) = binWtPair
				val binRslt = binOp.getBinIntrinsicResult(bin)
				(binRslt, wt)
			})
			val zero = binOp.zeroResult
			val combinedRsltJob: UIO[Rslt] = subResultWtPairs.runFold(zero)((prevRslt, pair) => {
				val (binRslt, wt) = pair
				binOp.combineResults(prevRslt, wt, binRslt)
			})
			combinedRsltJob
		}
	}

	class BinOp_Mean(keys: Seq[EntryKey]) extends BinOperation[EVec[EntryMean]] {
		// Is EVec[X] like Seq[X], with no keys?  Or is it more like a Map[EntryKey, X]?
		override def getBinIntrinsicResult(binHandle: BinHandle): EVec[EntryMean] = ???

		override def zeroResult: EVec[EntryMean] = ???

		override def combineResults(prevAccum: EVec[EntryMean], nextWt: BinRelWeight, nxtPartial: EVec[EntryMean]): EVec[EntryMean] = ???

		override def makeWeightedWrapper: WeightedWrapper = ???

		override protected def mkWeightedResult(binRelWt: BinRelWt, unwtRslt: EVec[EntryMean]): WeightedRslt = ???

		override def wrCombine(wr1: (BinRelWeight, WeightedRslt), wr2: (BinRelWeight, WeightedRslt)): (BinRelWeight, WeightedRslt) = ???
	}

	abstract class BinOp_ExpectedPairwiseProductMatrix(keys: Seq[EntryKey]) extends BinOperation[EMatrix[EntryPairProduct]]

	abstract class BinOp_CovarMatrix(keys: Seq[EntryKey]) extends BinOperation[EMatrix[EntryPairCovar]] {
		// How do we make use of a sub-operation?
		val prodMatrixOp = ???
	}


	trait BinTransformer {
		def transform(binHandle: BinHandle) : BinHandle
	}

	// Apply a vector of entry coefficients, creating a new named entry, which has statistics
	// (e.g. Sam's portfolio total return has mean, variance, and co-variance with other entries).
	abstract class BinXform_AddLinearComboEntry(expansionKey: EntryKey, coeffs: Seq[(EntryKey, EntryMult)]) extends BinTransformer {
	}
}
