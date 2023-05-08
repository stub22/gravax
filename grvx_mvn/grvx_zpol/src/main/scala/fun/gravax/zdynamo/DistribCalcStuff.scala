package fun.gravax.zdynamo

private trait DistribCalcStuff

trait KnowsDistribTypes extends NameScopeHmm {
	type Probability = BigDecimal // between 0.0 and 1.0
	type ProbDensity = BigDecimal // Positive value, representing probability density per unit volume.

	type PointEntry = (EntryKey, EntryValue)
	type PointRow = Seq[PointEntry] // This is a vector in the space identified by the chosen keys.

	type UnwtCov = BigDecimal
	type UnwtCovPair = (EntryKey, UnwtCov)
	type UnwtCovRow = IndexedSeq[UnwtCovPair]

	// ._2 entry in some bin (having wt _1) has covariances ._3 (w.r.t. GLOBAL mean, already known).
	type BinEntryMidCalc = (DBinWt, StatEntry, UnwtCovRow)
	type BinEntryMidNarr = (DBinWt, StatEntry)

	// type DownsideUCR = IndexedSeq[UnwtCovPair]
	type WtCov = BigDecimal
	type WtCovTup = (EntryKey, EntryKey, WtCov)
	type WtCovRow = IndexedSeq[WtCovTup]

	type StatTriMatrix = IndexedSeq[(StatEntry, WtCovRow)]
}
trait StatEntryOps extends KnowsDistribTypes {
	// Useful when pooling variances across bins.  expectedSquare = mean-squared plus variance
	def expectedSquare(entry : StatEntry) = entry._2.pow(2) + entry._3
	def wtdExpSqrAndMean(weight : DBinWt, entry : StatEntry) : WtdSqrAndMean = {
		val expSqr = expectedSquare(entry)
		val entMean = entry._2
		(weight.*(expSqr), weight.*(entMean))
	}
	def squareMeans() = ???
	def pooledVariance = ???

	def mkZeroCovRow = ???

	val zeroBD = BigDecimal("0.0")
	val oneBD = BigDecimal("1.0")
}
trait BinStatCalcs extends KnowsDistribTypes {
	val myStatEntryOps = new StatEntryOps {}
	def calcPooledMeanAndVar(statTupsForOneEntry: IndexedSeq[BinEntryMidNarr], storedRootEntryMean: EntryMean): StatEntry = {
		// TODO:  Assert prove all entryKeys equal, or factor out.
		val firstEntryKey : EntryKey = statTupsForOneEntry.head._2._1

		val sumOfWeights : DBinWt = statTupsForOneEntry.map(_._1).sum
		val wtSquaresAndMeans: Seq[WtdSqrAndMean] = statTupsForOneEntry.map(wep => myStatEntryOps.wtdExpSqrAndMean(wep._1, wep._2))

		val summedPairs: WtdSqrAndMean = wtSquaresAndMeans.reduce((pair1, pair2) => (pair1._1 + pair2._1, pair1._2 + pair2._2))
		val (sumOfWtdSqrs, sumOfWtdMeans) = summedPairs
		assert(sumOfWtdMeans == storedRootEntryMean) // Expecting this to fail, then we will go deeper in 'numerology'
		val squaredMeanCanned = storedRootEntryMean.pow(2)
		val squaredMeanOrganic = sumOfWtdMeans.pow(2) // == should be same if we use storedMean or sumOfWtdMeans
		assert(squaredMeanCanned == squaredMeanOrganic)
		// We expect sumOfWeights to be 1, so this division step can go away
		val normSqrs = sumOfWtdSqrs./(sumOfWeights)
		// https://stats.stackexchange.com/questions/43159/how-to-calculate-pooled-variance-of-two-or-more-groups-given-known-group-varianc
		val pooledVar = normSqrs.-(squaredMeanOrganic) // TA-DA!!!
		// assert(pooledVar == storedRootEntryVar)
		// JDK9+ has sqrt on BigDecimal. From Scala 2.13 we may have to use Spire or access the Java object, or ...
		// val pooledStdDev = pooledVar.sqrt(mc)
		// val jbd = pooledVar.underlying() // Gets the Java BD
		val pooledEntry : StatEntry = (firstEntryKey, sumOfWtdMeans, pooledVar )
		pooledEntry
	}
	def setupCovTup(dbd : DBinDat, eidx : Int, keySyms: IndexedSeq[EntryKey], storedRootMeanVec: IndexedSeq[EntryMean]): BinEntryMidCalc = {

		val covPartnerEntIdx: IndexedSeq[Int] = eidx + 1 to keySyms.size - 1
		val wt = dbd._2
		val statRow = dbd._3
		val localEntry = statRow(eidx)
		val localMean = localEntry._2
		// Naive formulation of "Excess" i.e. the deviation of this bin-mean from the global-mean.
		val storedRootEntryMean: EntryMean = storedRootMeanVec(eidx)
		val localExc = localMean.-(storedRootEntryMean)
		val covShortRow: UnwtCovRow = covPartnerEntIdx.map(coentIdx => {
			val coentKey = keySyms(coentIdx)
			val coRootMean = storedRootMeanVec(coentIdx)
			val coLocalEntry = statRow(coentIdx)
			val coLocalMean = coLocalEntry._2
			val coLocalExc = coLocalMean.-(coRootMean) // This gets computed entry-count times.  Could store in stat entry.
			val localCovar = localExc.*(coLocalExc) // Covariance estimated for key-pair within this bin
			(coentKey, localCovar)
		})
		(wt, localEntry, covShortRow)
	}
	// type WtCovTup = (EntryKey, EntryKey, WtCov)
	// wtEntStatTups tells the covariance row for each bins.  We do the weighted sum of those covariance rows.
	// TODO:  Derive the emptyShortRowWtCov from wtEntStatTups.head
	def completeShortCovRow(wtEntStatTups: IndexedSeq[(DBinWt, StatEntry, UnwtCovRow)], emptyShortRowWtCov : WtCovRow): WtCovRow = {
		// Total up the input rows to produce a single row of covariances - all the covariances for entry.
		// val emptyShortRowWtCov : WtCovRow = covPartnerEntIdx.map(cpeidx => (ekey, keySyms(cpeidx), zeroBD))
		val totalShortCovRow : WtCovRow = wtEntStatTups.foldLeft(emptyShortRowWtCov)((prevSumRow, wtStatTup) => {
			val rowWt = wtStatTup._1 // This weight came from the bin corresponding to this row
			val covShortRow: UnwtCovRow = wtStatTup._3
			val comb: IndexedSeq[(WtCovTup, UnwtCovPair)] = prevSumRow.zip(covShortRow)
			val summedShortRow : WtCovRow = comb.map(comboPair => {
				val (prevSumTup, nxtInPair) = comboPair
				val nxtInWeighted = nxtInPair._2.*(rowWt)
				val nxtSum = prevSumTup._3.+(nxtInWeighted)
				(prevSumTup._1, prevSumTup._2, nxtSum)
			})
			summedShortRow
		})
		totalShortCovRow
	}
}
