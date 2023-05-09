package fun.gravax.zdynamo

private trait BinStatStuff

trait BinSummaryCalc extends KnowsDistribTypes  {

	val myBinStatCalcs = new BinStatCalcs {}

	def calcParentStats(keySyms: IndexedSeq[EntryKey], storedRootMeanVec: IndexedSeq[EntryMean],
				binDataMatrix : IndexedSeq[(ParentTag, BinTag, DBinDat)]): IndexedSeq[(ParentTag, StatRow)] = {
		// Stored mean and statistics of the root bin
		// We always get

		val entryIdx = 0 to keySyms.size - 1
		val deepBinIdx = 0 to binDataMatrix.size - 1

		//	val sumOfWeights = ??? - Only needed in final pooling, where it can be derived locally.
		// sumOfWeights COULD be passed in as an optimization and extra check.
		// When our rootBin is a complete valid P.D., sumOfWeights should be 1.0

		// OUTER-looping through selected keysyms.
		// These output what we may visualize as the rows of an upper-triangular covariance matrix.
		// The StatEntry values provide the *diagonal* of the output matrix (the entry self-variances), and also
		// the means (which we may visualize as a separate column vector).
		val outStatsPerKey: IndexedSeq[(StatEntry, WtCovRow)] = entryIdx.map(eidx => {
			val ekey = keySyms(eidx) // Used in here only for labeling/debugging
			// We already have stored values for the mean and variance of this entry.
			val storedRootEntryMean: EntryMean = storedRootMeanVec(eidx)

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
				// Recall that projectedDeepBins is a cached array. This step is array.apply(), not a method call.
				val (parentTag, binTag, dBinDat) = binDataMatrix(bidx)
				val dbd: (DBinID, DBinWt, StatRow) = dBinDat
				// Do the gritty estimation of variance for focus entry (dbd/eidx) and covariance for short-row
				// of partner entries.  Those covariances require the global mean for both entries.
				myBinStatCalcs.beginCovXprod(dbd, eidx, keySyms, storedRootMeanVec)
			})

			val narrowerStatTups: IndexedSeq[BinEntryMidNarr]= wtEntStatTups.map(wideTup => (wideTup._1, wideTup._2))

			val pmav = myBinStatCalcs.calcAggregateMeanAndVar(narrowerStatTups, storedRootEntryMean)

			// JDK9+ has sqrt on BigDecimal. From Scala 2.13 we may have to use Spire or access the Java object, or ...
			// val pooledStdDev = pooledVar.sqrt(mc)
			// val jbd = pooledVar.underlying() // Gets the Java BD

			val outStat: StatEntry = pmav  // (ekey, sumOfWtdMeans, pooledVar)
			???
			// (outStat) // should be same as the stored stat (for this key) in the parent bin.
		})

		val weightedAvgOfBinMeans: Seq[EntryMean] = outStatsPerKey.map(_._1._2)
		assert(weightedAvgOfBinMeans == storedRootMeanVec)
		outStatsPerKey
		???
	}

}