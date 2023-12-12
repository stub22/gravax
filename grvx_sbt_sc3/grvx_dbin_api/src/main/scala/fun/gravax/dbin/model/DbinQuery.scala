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
// To define it we need to know:
// a) Intermediate result type to be combined
// Apply entryMedian over ...

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
trait BinQueryCtx {

	type BinHandle // How we hand a bin to an operation
	type BinWeight

	// type PartialResult  // What the operation hands back
	// type PRWtPair = (PartialResult, BinWeight)

	type EntryMean
	type EntryPairProduct
	type EntryPairCovar
	type EVec[X]
	type EMatrix[X]

	type EntryMult // e.g. fraction of portfolio

	// Defines a mapReduce operation in terms of required constituent operations
	trait BinOperation[Rslt] {
		// Asks a bin for some of its entries, and does some calculation with them.
		// A bin does not know its own weight!
		def getPartialResult(binHandle: BinHandle): Rslt

		// Does a monoid-like combine for some input STREAM of (partialResult, weight) pairs
		// When all the Partial Results have been combined, we have an answer.

		def zeroResult: Rslt

		def combineResults(prevAccum: Rslt, nextWt: BinWeight, nxtPartial: Rslt): Rslt
	}


	trait BinQueryApi[F[_]] {
		def getBinTag(bin: BinHandle): BinTag

		def getParentTag(bin: BinHandle): Option[BinTag]

		// Some entries may need calculation, probably via some binOperation
		def getEntries(bin: BinHandle)(keys: Set[EntryKey]): F[Map[EntryKey, BinEntry]]

		def applyBinOperation[Rslt](binOp: BinOperation[Rslt])(bin: BinHandle, subLevelNum: Int): F[Rslt]

		def applyBinTransform(binXform : BinTransformer)(bin: BinHandle) : BinHandle = binXform.transform(bin)

	}

	trait BinDataZStreamer {
		def streamDescendantLevel(bin: BinHandle, subLevelNum: Int): UStream[(BinHandle, BinWeight)]
	}

	class BQZApi(bdz: BinDataZStreamer) extends BinQueryApi[Task] {
		override def getBinTag(bin: BinHandle): BinTag = ???

		override def getParentTag(bin: BinHandle): Option[BinTag] = ???

		override def getEntries(bin: BinHandle)(keys: Set[EntryKey]): Task[Map[EntryKey, BinEntry]] = {
			???
		}

		override def applyBinOperation[Rslt](binOp: BinOperation[Rslt])(bin: BinHandle, subLevelNum: Int): Task[Rslt] = {
			val subBinWtPairs = bdz.streamDescendantLevel(bin, subLevelNum)
			val subResultWtPairs: UStream[(Rslt, BinWeight)] = subBinWtPairs.map(binWtPair => {
				val (bin, wt) = binWtPair
				val binRslt = binOp.getPartialResult(bin)
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
		override def getPartialResult(binHandle: BinHandle): EVec[EntryMean] = ???

		override def zeroResult: EVec[EntryMean] = ???

		override def combineResults(prevAccum: EVec[EntryMean], nextWt: BinWeight, nxtPartial: EVec[EntryMean]): EVec[EntryMean] = ???
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
	class BinXform_AddLinearComboEntry(expansionKey: EntryKey, coeffs: Seq[(EntryKey, EntryMult)]) extends BinTransformer {
	}
}
