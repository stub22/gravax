package fun.gravax.zdynamo

private trait DistribImplStuff


trait VecDistribFragment extends NameScopeHmm {
	type UnwtCov = BigDecimal
	type UnwtCovPair = (EntryKey, UnwtCov)
	type UnwtCovRow = IndexedSeq[UnwtCovPair]
	// type DownsideUCR = IndexedSeq[UnwtCovPair]
	type WtCov = BigDecimal
	type WtCovTup = (EntryKey, EntryKey, WtCov)
	type WtCovRow = IndexedSeq[WtCovTup]

	type StatTriMatrix = IndexedSeq[(StatEntry, WtCovRow)]

	type PointEntry = (EntryKey, EntryValue)
	type PointRow = Seq[PointEntry] // This is a vector in the space identified by the chosen keys.

	// We expect Assets (meatKeys) to be identical across all bins
	def	getFullKeySymSet : Set[EntryKey] // The syms do not have a canonical ordering.  Client may use alphabetic, or...
	def projectStatRow(keySyms : IndexedSeq[EntryKey]) : StatRow // Often this is available directly from VecDistrib-bin-0

}

trait StatEntryOps extends NameScopeHmm {
	// Useful when pooling variances across bins.  expectedSquare = mean-squared plus variance
	def expectedSquare(entry : StatEntry) = entry._2.pow(2) + entry._3
	def wtdExpSqrAndMean(weight : DBinWt, entry : StatEntry) : WtdSqrAndMean = {
		val expSqr = expectedSquare(entry)
		(weight.*(expSqr), weight.*(entry._2))
	}
	def squareMeans() = ???
	def pooledVariance = ???

	def mkZeroCovRow = ???
}


// Distribution does not encode any canonical ordering of the Entry dimensions.
// The ordering choice always comes from the client, via a sequence argument to our methods.
trait VecDistrib extends VecDistribFragment {

	// TODO:  Refine to a triangular rep.
	type CovarRow = (EntryKey, StatRow)
	type CovarMatrix = IndexedSeq[CovarRow]

	// Produce a covariance matrix for the given keys (only), using at most maxLevels of the distro-bin structure
	def projectEstimCovars(keySyms : IndexedSeq[EntryKey], maxLevels : Int) : StatTriMatrix

	// CDF of the marginal distribution of our VecDistrib's projection into the dimensions of point's keys.
	def projEstimMarginalCDF(point : PointRow, maxLevels : Int) : Probability = ???

	def projEstimMarginalPDF(point : PointRow, maxLevels : Int) : ProbDensity = ???

	// Project all data of the distribution for the given keys only, into a new instance of this type.
	// We COULD use this to scope the project/marginal operations and simplify the other signatures.
	def projectMarginalDistrib_DoWeNeedThis(keySyms : Seq[EntryKey]) : VecDistrib = ???

	// Pull out the data of the root bin for the given keys (only)
	def projectRootBin(keySyms : IndexedSeq[EntryKey]) : DBinDat

	// Pull out the data of all direct children of the given bin, for the given keys only.
	def projectChildBins(parentBinID : DBinID, keySyms : IndexedSeq[EntryKey]) : Seq[DBinDat] = ???

}


// Assume meatKeys are same across all bins
class VecDistribBinnedImpl(rootBN : BinNode) extends VecDistrib {
	override def getFullKeySymSet: Set[EntryKey] = rootBN.getFullKeySymSet

	override def projectStatRow(keySyms: IndexedSeq[EntryKey]): StatRow = rootBN.projectStatRow(keySyms)

	// Produces same result with another level of wrapper that includes binID and relWeight (which must be 1.0?)
	override def projectRootBin(keySyms: IndexedSeq[EntryKey]): DBinDat = rootBN.projectToDBD(keySyms)

	// 1 level  => only the marginal self-variances stored in the root bin
	// 2 levels => approximate covariance using local-means (only!) of the root-kids (+ global means) for all the off-diagonal elements.
	// on-diagonal are computed using pooled-variance formula from marginal-variances (+ local-means + global-means) in the root-kids.
	// These latter on-diagonal sums should come out to the same as the root's stored self-variances (and this is where
	// those values come from in a bottom-up assembly of bins).

	// TODO:  Also capture downside variance and downside covariance IF these can actually be used for sortino ratio?
	// Or would we be better off trying to work directly with distribution of portfolio returns?
	// We have not yet decided how to incorporate variance-balls into the portfolio composite distro.
	// Perhaps would need to assume normal distro for each bin.


	val statEntryOps = new StatEntryOps {}

	override def projectEstimCovars(keySyms: IndexedSeq[EntryKey], maxLevels: Int): StatTriMatrix = {
		// We always get
		val rootStatRow: IndexedSeq[StatEntry] = projectStatRow(keySyms)
		val storedMeanVec: IndexedSeq[EntryMean] = rootStatRow.map(_._2)
		val storedVarVec: IndexedSeq[EntryVar] = rootStatRow.map(_._3)
		val fullStatOut : StatTriMatrix = if (maxLevels == 1) {
			// Return matrix that is nonzero only on the diagonal?
			// Or define a subtype of CovarMatrix that is Variance-only
			???
		} else {
			val projectedDeepBins: DBinMatrix = rootBN.projectAndCollectBins(keySyms, maxLevels)
			val entryIdx = 0 to keySyms.size - 1
			val binIdx = 0 to projectedDeepBins.size - 1

			val sumOfWeights = ???
			val zeroBD = BigDecimal("0.0")
			val oneBD = BigDecimal("1.0")
			assert(sumOfWeights == oneBD)

			val outStatsPerKey: IndexedSeq[(StatEntry, WtCovRow)] = entryIdx.map(eidx => {
				val ekey = keySyms(eidx) // Used in here only for labeling/debugging
				// We already have stored values for the mean and variance of this entry.
				val storedMean = storedMeanVec(eidx)
				val storedVar = storedVarVec(eidx)

				// Covar is symmetric, so we only need the covariances with entries having a higher index than eidx.
				val covPartnerEntIdx = eidx + 1 to keySyms.size - 1
				// But let's calculate them anyway, hoping to get the same answer.
				// TODO:  Could fold these values in fewer steps, with less copying
				val wtEntStatTups: IndexedSeq[(DBinWt, StatEntry, UnwtCovRow)] = binIdx.map(bidx => {
					val dbd = projectedDeepBins(bidx)
					val wt = dbd._2
					val statRow = dbd._3
					val localEntry = statRow(eidx)
					val localMean = localEntry._2
					// Naive formulation of "Excess" i.e. the deviation of this bin-mean from the global-mean.
					val localExc = localMean.-(storedMean)
					val covShortRow: UnwtCovRow = covPartnerEntIdx.map(coentIdx => {
						val coentKey = keySyms(coentIdx)
						val coGlobalMean = storedMeanVec(coentIdx)
						val coLocalEntry = statRow(coentIdx)
						val coLocalMean = coLocalEntry._2
						val coLocalExc = coLocalMean.-(coGlobalMean) // This gets computed entry-count times.  Could store in stat entry.
						val localCovar = localExc.*(coLocalExc) // Covariance estimated for key-pair within this bin
						(coentKey, localCovar)
					})
					(wt, localEntry, covShortRow)
				})
				// Total up the input rows to produce a single row of covariances - all the covariances for entry.
				val emptyShortRowWtCov : WtCovRow = covPartnerEntIdx.map(cpeidx => (ekey, keySyms(cpeidx), zeroBD))
				val totalShorCovRow : WtCovRow = wtEntStatTups.foldLeft(emptyShortRowWtCov)((prevSumRow, wtStatTup) => {
					val rowWt = wtStatTup._1 // This weight came from the bin corresponding to this row
					val covShortRow: UnwtCovRow = wtStatTup._3
					val zzz: WtCovRow = prevSumRow
					val mmm = zzz.zip(covShortRow)
					val comb: IndexedSeq[(WtCovTup, UnwtCovPair)] = prevSumRow.zip(covShortRow)
					val summedShortRow : WtCovRow = comb.map(comboPair => {
						val (prevSumTup, nxtInPair) = comboPair
						val nxtInWeighted = nxtInPair._2.*(rowWt)
						val nxtSum = prevSumTup._3.+(nxtInWeighted)
						(prevSumTup._1, prevSumTup._2, nxtSum)
					})
					summedShortRow
				})
				val wtSquaresAndMeans: Seq[WtdSqrAndMean] = wtEntStatTups.map(wep => statEntryOps.wtdExpSqrAndMean(wep._1, wep._2))

				val summedPairs: WtdSqrAndMean = wtSquaresAndMeans.reduce((pair1, pair2) => (pair1._1 + pair2._1, pair1._2 + pair2._2))
				val (sumOfWtdSqrs, sumOfWtdMeans) = summedPairs
				assert(sumOfWtdMeans == storedMean) // Expecting this to fail, then we will go deeper in 'numerology'
				val squaredMeanCanned = storedMean.pow(2)
				val squaredMeanOrganic = sumOfWtdMeans.pow(2) // == should be same if we use storedMean or sumOfWtdMeans
				assert(squaredMeanCanned == squaredMeanOrganic)
				// We expect sumOfWeights to be 1, so this division step can go away
				val normSqrs = sumOfWtdSqrs./(sumOfWeights)
				// https://stats.stackexchange.com/questions/43159/how-to-calculate-pooled-variance-of-two-or-more-groups-given-known-group-varianc
				val pooledVar = normSqrs.-(squaredMeanOrganic) // TA-DA!!!
				assert(pooledVar == storedVar)
				// JDK9+ has sqrt on BigDecimal. From Scala 2.13 we may have to use Spire or access the Java object, or ...
				// val pooledStdDev = pooledVar.sqrt(mc)
				// val jbd = pooledVar.underlying() // Gets the Java BD

				val outStat: StatEntry = (ekey, sumOfWtdMeans, pooledVar)
				(outStat, totalShorCovRow) // should be same as the stored stat (for this key) in the parent bin.
			})


			val weightedAvgOfBinMeans: Seq[EntryMean] = ???
			assert(weightedAvgOfBinMeans == storedMeanVec)
			outStatsPerKey
		}
		fullStatOut
	}

}