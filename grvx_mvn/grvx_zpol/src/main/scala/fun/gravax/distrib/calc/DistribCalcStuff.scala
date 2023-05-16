package fun.gravax.distrib.calc

import fun.gravax.distrib.struct.StatTupleShapes

private trait DistribCalcStuff

trait KnowsDistribTypes extends StatTupleShapes {

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
	val zeroBD = BigDecimal("0.0")
	val oneBD = BigDecimal("1.0")

	// Useful when pooling variances across bins.  expectedSquare = mean-squared plus variance
	def expectedSquare(entry : StatEntry) = (entry._2.pow(2)).+(entry._3)
	// Before we were passing the args separately, then it helps to do:  val helpFunc = (myStatEntryOps.entryFromExpects _).tupled
	def entryFromExpects(entryExp : EntryExpects) : StatEntry = {
		val (entryKey, entryMean, expSq) = entryExp
		val entryVar = expSq.-(entryMean.pow(2))
		val stEntry : StatEntry = (entryKey, entryMean, entryVar)
		stEntry
	}
	def expectsFromEntry(entry : StatEntry) : EntryExpects = {
		val expSq = expectedSquare(entry)
		val (entKey, entMean, entVar) = entry
		val entryExp : EntryExpects = (entKey, entMean, expSq)
		entryExp
	}
	def wtdExpSqrAndMean(weight : DBinWt, entry : StatEntry) : WtdSqrAndMean = {
		val expSqr = expectedSquare(entry)
		val entMean = entry._2
		(weight.*(expSqr), weight.*(entMean))
	}

	// Weighted expect-rows (which use the same entry-keys in same order) are monoidally combinable using this op.
	// (Technically are only using their semigroup property, because we don't have a reason to use zero-rows, yet).
	// Output weight is sum of input weights.
	// Output expects-row is weighted avg of input expects rows.
	// Input and output rows should have same entry-keys in same order. (Enforced by assertion).
	def combineWeightedExpectationRows(rowA : WtExpectsRow, rowB : WtExpectsRow) : WtExpectsRow = {
		val (wtA, expEntsA) = rowA
		val (wtB, expEntsB) = rowB
		val wtSum = wtA.+(wtB)
		assert(expEntsA.size == expEntsB.size)
		val joinedEnts: IndexedSeq[(EntryExpects, EntryExpects)] = expEntsA.zip(expEntsB)
		val mergedExpects : IndexedSeq[EntryExpects] = joinedEnts.map(tup => {
			val (eeA, eeB) = tup
			val (keyA, meanA, expSqA) = eeA
			val (keyB, meanB, expSqB) = eeB
			assert(keyA == keyB)
			val wtSumMean = wtA.*(meanA).+(wtB.*(meanB))
			val wtSumExpSq = wtA.*(expSqA).+(wtB.*(expSqB))
			val outMean = wtSumMean./(wtSum)
			val outExpSq = wtSumExpSq./(wtSum)
			val outKey = keyA
			val outExpects = (outKey, outMean, outExpSq)
			outExpects
		})
		val outRow = (wtSum, mergedExpects)
		outRow
	}
}
trait BinStatCalcs extends KnowsDistribTypes {
	val myStatEntryOps = new StatEntryOps {}

	// type BinRDat = (DBinWt, StatRow)
	def aggregateWeightsMeansAndVars(binDats : Iterable[DBinDat]) : (DBinWt, StatRow) = {
		val weightedExpects : WtExpectsRow = reduceWeightedExpectations(binDats)
		val (rowWt, eeSeq) = weightedExpects
		val statRow : StatRow = eeSeq.map(ee => myStatEntryOps.entryFromExpects(ee))
		(rowWt, statRow)
	}
	def reduceWeightedExpectations(binDats : Iterable[DBinDat]) : WtExpectsRow = {
		val inBinRows: Iterable[WtExpectsRow] = binDats.map(binDat => {
			val(binId, binWt, statRow) = binDat
			val expectsSeq : IndexedSeq[EntryExpects] = statRow.map(stEnt => {
				val (entKey, entMean, entVar) = stEnt
				val expSq = myStatEntryOps.expectedSquare(stEnt)
				val ee : EntryExpects = (entKey, entMean, expSq)
				ee
			})
			val wtExpRow = (binWt, expectsSeq)
			wtExpRow
		})
		inBinRows.reduce((werA, werB) => myStatEntryOps.combineWeightedExpectationRows(werA, werB))
	}

	def calcAggregateMeanAndVar(statTupsForOneEntry: IndexedSeq[BinEntryMidNarr], storedRootEntryMean: EntryMean): StatEntry = {
		// TODO:  Assert prove all entryKeys equal, or factor out.
		val firstEntryKey : EntryKey = statTupsForOneEntry.head._2._1

		// TODO:  Confirm that Seq[BigDecimal].sum works correctly.
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
		val aggregateEntry : StatEntry = (firstEntryKey, sumOfWtdMeans, pooledVar )
		aggregateEntry
	}
	def beginCovXprod(dbd : DBinDat, eidx : Int, keySyms: IndexedSeq[EntryKey], storedRootMeanVec: IndexedSeq[EntryMean]): BinEntryMidCalc = {

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
	def finishShortCovRow(wtEntStatTups: IndexedSeq[(DBinWt, StatEntry, UnwtCovRow)], emptyShortRowWtCov : WtCovRow): WtCovRow = {
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
trait TooManyTypes {
	type PairFunc[L,R]=Function0[(L,R)]
	type WeirdFunc[A,B,C,D] = (A,D) => (B,C)
	// type RPair=(String,Iterable[])

}