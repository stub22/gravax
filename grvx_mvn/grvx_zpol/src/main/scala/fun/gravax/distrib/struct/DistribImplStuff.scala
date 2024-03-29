package fun.gravax.distrib.struct

import fun.gravax.distrib.calc.{BinStatCalcs, DBinStatClz, KnowsDistribTypes}
import zio.{Task, ZIO, Random => ZRandom}

private trait DistribImplStuff

trait VectorDistribTypes extends KnowsDistribTypes {
	type Probability = BigDecimal // between 0.0 and 1.0
	type ProbDensity = BigDecimal // Positive value, representing probability density per unit volume.

	type PointEntry = (EntryKey, EntryValue)
	type PointRow = Seq[PointEntry] // This is a vector in the space identified by the chosen keys.
}

trait VecDistribFragment extends VectorDistribTypes {
	// We expect Assets (meatKeys) to be identical across all bins
	// def	getFullKeySymSet : Set[EntryKey] // The syms do not have a canonical ordering.  Client may use alphabetic, or...
	def projectShallowStatRow(keySyms : IndexedSeq[EntryKey]) : Task[StatRow] // Often this is available directly from VecDistrib-bin-0
}

// Distribution does not encode any canonical ordering of the Entry dimensions.
// The ordering choice always comes from the client, via a sequence argument to our methods.
trait VecDistrib extends VecDistribFragment {

	// TODO:  Refine to a triangular rep.
	type CovarRow = (EntryKey, StatRow)
	type CovarMatrix = IndexedSeq[CovarRow]

	// Produce a covariance matrix for the given keys (only), using at most maxLevels of the distro-bin structure
	def projectEstimCovars(keySyms : IndexedSeq[EntryKey], maxLevels : Int) : Task[StatTriMatrix]

	// CDF of the marginal distribution of our VecDistrib's projection into the dimensions of point's keys.
	def projEstimMarginalCDF(point : PointRow, maxLevels : Int) : Probability = ???

	def projEstimMarginalPDF(point : PointRow, maxLevels : Int) : ProbDensity = ???

	// Project all data of the distribution for the given keys only, into a new instance of this type.
	// We COULD use this to scope the project/marginal operations and simplify the other signatures.
	// def projectMarginalDistrib_DoWeNeedThis(keySyms : Seq[EntryKey]) : VecDistrib = ???

	// Pull out the data of the root bin for the given keys (only)
	def projectRootBin(keySyms : IndexedSeq[EntryKey]) : Task[DBinStatClz]

	// Pull out the data of all direct children of the given bin, for the given keys only.
	def projectChildBins(parentBinID : DBinID, keySyms : IndexedSeq[EntryKey]) : Task[IndexedSeq[DBinStatClz]] = ???

}

// Assume meatKeys are same across all bins.
// TODO:  We need protection against lopsided trees, especially incomplete groups of immediate siblings which can lead to WRONG answers.
// Incomplete sibling groups can arise when the tree load was truncated by numBins.
class VecDistribBinnedImpl(rootBN : BinNode) extends VecDistrib {
	// override def getFullKeySymSet: Set[EntryKey] = rootBN.getFullKeySymSet

	override def projectShallowStatRow(keySyms: IndexedSeq[EntryKey]): Task[StatRow] = rootBN.projectShallowStatRow(keySyms)

	// Produces same result, inside the DBinStatClz wrapper
	override def projectRootBin(keySyms: IndexedSeq[EntryKey]): Task[DBinStatClz] = rootBN.projectToDBSC_op(keySyms)

	// 1 level  => only the marginal self-variances stored in the root bin
	// 2 levels => approximate covariance using local-means (only!) of the root-getKids (+ global means) for all the off-diagonal elements.
	// on-diagonal are computed using pooled-variance formula from marginal-variances (+ local-means + global-means) in the root-getKids.
	// These latter on-diagonal sums should come out to the same as the root's stored self-variances (and this is where
	// those values come from in a bottom-up assembly of bins).

	// TODO:  Also capture downside variance and downside covariance IF these can actually be used for sortino ratio?
	// Or would we be better off trying to work directly with distribution of portfolio returns?
	// We have not yet decided how to incorporate variance-balls into the portfolio composite distro.
	// Perhaps would need to assume normal distro for each bin.

	val myBinStatCalcs = new BinStatCalcs {}

	case class SummedWeightedSquares(sumOfWtSqMns : BigDecimal, sumOfWtVrncs : BigDecimal)

	// type StatTriMatrix = IndexedSeq[(StatEntry, VwtCovRow)]
	override def projectEstimCovars(keySyms: IndexedSeq[EntryKey], maxLevels: Int): Task[StatTriMatrix] = {
		// Stored mean and statistics of the root bin
		// We always get
		val rootStatRow_op: Task[StatRow] = projectShallowStatRow(keySyms)
		val rootMass = rootBN.getDirectMass
		val queryDepth = maxLevels - 1 // Our root counts as "level 1".  Query depth is number of levels to dig beyond root.

		val projectedDeepBins_op: Task[DBinStatMatrix] = rootBN.projectAndCollectBins(keySyms, queryDepth)
		val bothOp: Task[(IndexedSeq[(EntryKey, EntryMean, EntryVar)], DBinStatMatrix)] = rootStatRow_op.zip(projectedDeepBins_op)
		val matrixTask: Task[StatTriMatrix] = bothOp.map(pair => {
			val (rootStatRow, projectedDeepBinStats) = pair
			println(s"deepBinStats.size=${projectedDeepBinStats.size}, rootStatRow=${rootStatRow}")
			val storedRootMeanVec: IndexedSeq[EntryMean] = rootStatRow.map(_._2)
			val storedRootVarVec: IndexedSeq[EntryVar] = rootStatRow.map(_._3)
			val fullStatOut : StatTriMatrix = if (maxLevels == 1) {
				// Return matrix that is nonzero only on the diagonal?
				// Or define a subtype of CovarMatrix that is Variance-only
				???
			} else {
				val entryIdx = 0 to keySyms.size - 1
				val deepBinIdx = 0 to projectedDeepBinStats.size - 1

				//	val sumOfWeights = ??? - Only needed in final pooling, where it can be derived locally.
				// sumOfWeights COULD be passed in as an optimization and extra check.
				// When our rootBin is a complete valid P.D., sumOfWeights should be 1.0

				// OUTER-looping through selected keysyms.
				// These output what we may visualize as the rows of an upper-triangular covariance matrix.
				// The StatEntry values provide the *diagonal* of the output matrix (the entry self-variances), and also
				// the means (which we may visualize as a separate column vector).
				val outStatsPerKey: IndexedSeq[(StatEntry, VwtCovRow)] = entryIdx.map(eidx => {
					val ekey = keySyms(eidx) // Used in here only for labeling/debugging
					// We already have stored values for the mean and variance of this entry.
					val storedRootEntryMean: EntryMean = storedRootMeanVec(eidx)
					val storedRootEntryVar: EntryVar = storedRootVarVec(eidx)

					// Covar is symmetric, so we only need the covariances with entries having a higher index than eidx.
					val kss = keySyms.size
					val maxEIDX = kss - 1

					val covPartnerEntIdx: IndexedSeq[Int] = if (eidx < maxEIDX) {
						eidx + 1 to maxEIDX
					} else IndexedSeq()
					// But let's calculate them anyway, hoping to get the same answer.
					// FIXME:  Could fold these values in fewer steps, with less copying
					// INNER-looping:  We iterate over the deep-expanded bin sequence, computing one of these tuples for
					// each bin found at maxLevels. Each output tuple contains stats and one short-row of covars, for one bin.
					val wtEntStatTups: IndexedSeq[BinEntryMidCalc] = deepBinIdx.map(bidx => {
						// Recall that projectedDeepBinStats is a cached array. This step is array.apply(), not a method call.
						val binStat = projectedDeepBinStats(bidx)
						// Do the gritty estimation of variance for focus entry (binStat/eidx) and covariance for short-row
						// of partner entries.  Those covariances require the global mean for both entries.
						myBinStatCalcs.beginCovXprod(binStat, eidx, keySyms, storedRootMeanVec)
					})

					// Total up the input rows to produce a single row of covariances - all the covariances for entry
					// Hmm, if covPartnerEntIdx is EMPTY, then emptyShortRowWtCov will be empty.
					val outShortCovRow = if (covPartnerEntIdx.isEmpty) IndexedSeq() else {
						val emptyShortRowWtCov: VwtCovRow = covPartnerEntIdx.map(cpeidx => (ekey, keySyms(cpeidx), myBinStatCalcs.myStatEntryOps.zeroBD))
						val totalShortCovRow: VwtCovRow = myBinStatCalcs.finishShortCovRow(wtEntStatTups, emptyShortRowWtCov)
						totalShortCovRow
					}

					val narrowerStatTups: IndexedSeq[BinEntryMidNarr]= wtEntStatTups.map(wideTup => (wideTup._1, wideTup._2))

					val pmav : StatEntry = myBinStatCalcs.calcAggregateMeanAndVar(narrowerStatTups, storedRootEntryMean, rootMass)
					val (aggKey, aggMean, aggVar) = pmav
					val rootVarTolerance = BigDecimal("0.0000001")
					myBinStatCalcs.assertEqualWithinTolerance("aggVar", aggVar, "storedRootVar", storedRootEntryVar, rootVarTolerance )
					// assert(pmav._3 == storedRootEntryVar)

					// JDK9+ has sqrt on BigDecimal. From Scala 2.13 we may have to use Spire or access the Java object, or ...
					// val pooledStdDev = pooledVar.sqrt(mc)
					// val jbd = pooledVar.underlying() // Gets the Java BD

					val outStat: StatEntry = pmav  // (ekey, sumOfWtdMeans, pooledVar)
					(outStat, outShortCovRow) // should be same as the stored stat (for this key) in the parent bin.
				}) // end of outStatsPerKey

				val aggBinMeans: Seq[EntryMean] = outStatsPerKey.map(_._1._2)
				println(s"aggBinMeans=${aggBinMeans}")
				println(s"storedRootMeanVec=${storedRootMeanVec}")
				myBinStatCalcs.assertNumSeqsRoughlyEqual("aggBinMeans", aggBinMeans, "storedRootMeanVec", storedRootMeanVec, myBinStatCalcs.myEqTolerance)
				outStatsPerKey
			} // end of else
			fullStatOut // result of bothOp.map(pair => {
		}) // end of matrixTask
		matrixTask
	}
}

trait VecDistTestHelper extends KnowsDistribTypes {
	def computeCovars (rootBinNode : BinNode)(keySyms: Seq[EntryKey], maxLevels: Int) : Task[StatTriMatrix] = {
		val vecDistrib = new VecDistribBinnedImpl(rootBinNode)
		val keysIndexed = keySyms.toIndexedSeq
		val covarTask = vecDistrib.projectEstimCovars(keysIndexed, maxLevels)
		covarTask
	}

	def chooseSomeKeys(zrnd : ZRandom)(binNode : BinNode, maxKeys : Int) : Task[Seq[EntryKey]] = {
		val allKeysOp: Task[Seq[binNode.EntryKey]] = binNode.allKeysSorted
		allKeysOp.flatMap(sortedKeys => {
			val numKeys = sortedKeys.size
			val shuffledKeysOp = zrnd.shuffle(sortedKeys)
			shuffledKeysOp.map(shuffledKeys => shuffledKeys.take(maxKeys))
		})
	}
}



