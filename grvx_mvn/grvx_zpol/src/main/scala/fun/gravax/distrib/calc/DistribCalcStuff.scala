package fun.gravax.distrib.calc

import fun.gravax.distrib.struct.{KnowsStatTupleShapes}

private trait DistribCalcStuff

trait KnowsDistribTypes extends KnowsStatTupleShapes {

	type UnwtCov = BigDecimal
	type UnwtCovPair = (EntryKey, UnwtCov)
	type UnwtCovRow = IndexedSeq[UnwtCovPair]

	// ._2 entry in some bin (having wt _1) has covariances ._3 (w.r.t. GLOBAL mean, already known).
	type BinEntryMidCalc = (VagueWt, StatEntry, UnwtCovRow)
	type BinEntryMidNarr = (VagueWt, StatEntry)

	// type DownsideUCR = IndexedSeq[UnwtCovPair]
	type VwtCov = BigDecimal
	type VwtCovTup = (EntryKey, EntryKey, VwtCov)
	type VwtCovRow = IndexedSeq[VwtCovTup]

	type StatTriMatrix = IndexedSeq[(StatEntry, VwtCovRow)]
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
	def wtdExpSqrAndMean(vwt : VagueWt, entry : StatEntry) : VwtdSqrAndMean = {
		val expSqr = expectedSquare(entry)
		val entMean = entry._2
		(vwt.*(expSqr), vwt.*(entMean))
	}

	// Weighted expect-rows (which use the same entry-keys in same order) are monoidally combinable using this op.
	// (Technically are only using their semigroup property, because we don't have a reason to use zero-rows, yet).
	// Output vwt is sum of input weights.
	// Output expects-row is weighted avg of input expects rows.
	// Input and output rows should have same entry-keys in same order. (Enforced by assertion).
	def combineWeightedExpectationRows(rowA : VwtExpectsRow, rowB : VwtExpectsRow) : VwtExpectsRow = {
		val (vwtA, expEntsA) = rowA
		val (vwtB, expEntsB) = rowB
		val vwtSum = vwtA.+(vwtB)
		assert(expEntsA.size == expEntsB.size)
		val joinedEnts: IndexedSeq[(EntryExpects, EntryExpects)] = expEntsA.zip(expEntsB)
		val mergedExpects : IndexedSeq[EntryExpects] = joinedEnts.map(tup => {
			val (eeA, eeB) = tup
			val (keyA, meanA, expSqA) = eeA
			val (keyB, meanB, expSqB) = eeB
			assert(keyA == keyB)
			val wtSumMean = vwtA.*(meanA).+(vwtB.*(meanB))
			val wtSumExpSq = vwtA.*(expSqA).+(vwtB.*(expSqB))
			val outMean = wtSumMean./(vwtSum)
			val outExpSq = wtSumExpSq./(vwtSum)
			val outKey = keyA
			val outExpects = (outKey, outMean, outExpSq)
			outExpects
		})
		val outRow = (vwtSum, mergedExpects)
		outRow
	}
}
trait BinStatCalcs extends KnowsDistribTypes {
	val myStatEntryOps = new StatEntryOps {}

	// type BinRDat = (BinRelWt, StatRow)
	def aggregateWeightsMeansAndVars(binStats : Iterable[DBinStatClz]) : (VagueWt, StatRow) = {
		val weightedExpects : VwtExpectsRow = reduceWeightedExpectations(binStats)
		val (rowVwt, eeSeq) = weightedExpects
		val statRow : StatRow = eeSeq.map(ee => myStatEntryOps.entryFromExpects(ee))
		(rowVwt, statRow)
	}
	private def reduceWeightedExpectations(binStats : Iterable[DBinStatClz]) : VwtExpectsRow = {
		val inBinRows: Iterable[VwtExpectsRow] = binStats.map(binStat => {
			// val(binId, binWt, statRow) = binDat
			val binMass = binStat.massInfo.binMass
			val statRow = binStat.statRow
			val expectsSeq : IndexedSeq[EntryExpects] = statRow.map(stEnt => {
				val (entKey, entMean, entVar) = stEnt
				val expSq = myStatEntryOps.expectedSquare(stEnt)
				val ee : EntryExpects = (entKey, entMean, expSq)
				ee
			})
			val vagueWtInThisCaseIsMass = binMass
			val vwtExpRow = (vagueWtInThisCaseIsMass, expectsSeq)
			vwtExpRow
		})
		inBinRows.reduce((werA, werB) => myStatEntryOps.combineWeightedExpectationRows(werA, werB))
	}
	val myEqTolerance = BigDecimal("0.00000001")

	val myRootMassTol = BigDecimal("0.002") // Needs to be proportional to num bins?
	private def equalWithinTolerance(a : BigDecimal, b : BigDecimal, tol : BigDecimal) : Boolean =  {
		val absDiff = a.-(b).abs
		absDiff.compareTo(tol) < 0
	}
	def notEqualMsg(name01 : String, num01 : BigDecimal, name02 : String, num02 : BigDecimal, tol : BigDecimal) : String = {
		val absDiff = num01.-(num02).abs
		s"${name01}=${num01} != ${num02}=${name02}, absDiff=${absDiff} >= ${tol}=tolerance"
	}
	def assertEqualWithinTolerance(name01 : String, num01 : BigDecimal, name02 : String, num02 : BigDecimal, tol : BigDecimal) = {
		assert(equalWithinTolerance(num01, num02, tol), notEqualMsg(name01, num01, name02, num02, tol))
	}
	def assertNumSeqsRoughlyEqual(name01 : String, numSeq01 : Seq[BigDecimal], name02 : String, numSeq02 : Seq[BigDecimal], tol : BigDecimal) = {
		val numTups = numSeq01.zip(numSeq02).zipWithIndex
		numTups.foreach(tup => {
			val ((num01, num02), idx) = tup
			assertEqualWithinTolerance(s"${name01}_${idx}", num01, s"${name02}_${idx}", num02, tol)
		})
	}
	def calcAggregateMeanAndVar(statTupsForOneEntry: IndexedSeq[BinEntryMidNarr], storedRootEntryMean: EntryMean, storedRootMass : BinMass): StatEntry = {
		// TODO:  Assert prove all entryKeys equal, or factor out.
		val firstEntryKey : EntryKey = statTupsForOneEntry.head._2._1

		// TODO:  Confirm that Seq[BigDecimal].sum works correctly.
		val sumOfMasses : BinMass = statTupsForOneEntry.map(_._1).sum
		assert(sumOfMasses.signum > 0)
		// Using higher tolerance bc as of 2023-05-20:  sumOfMasses=66319.0597 != 66319.06=storedRootMass, absDiff=0.0003 >= 1E-9=tolerance
		assertEqualWithinTolerance("sumOfMasses", sumOfMasses, "storedRootMass", storedRootMass, myRootMassTol)
		println(s"calcAggregateMeanAndVar: statTupsForOneEntry=${statTupsForOneEntry}")
		println(s"calcAggregateMeanAndVar: firstEntryKey=${firstEntryKey} numTups=${statTupsForOneEntry.size} sumOfWeights=${sumOfMasses}")

		val wtSquaresAndMeans: Seq[VwtdSqrAndMean] = statTupsForOneEntry.map(wep => myStatEntryOps.wtdExpSqrAndMean(wep._1, wep._2))

		val summedPairs: VwtdSqrAndMean = wtSquaresAndMeans.reduce((pair1, pair2) => (pair1._1 + pair2._1, pair1._2 + pair2._2))
		val (sumOfWtdSqrs, sumOfWtdMeans) = summedPairs
		val aggMean = sumOfWtdMeans./(sumOfMasses)
		// Expecting this assert to fail, then we will go deeper in 'numerology'
		// aggMean=0.006056559939028333961737397793654182 != 0.0060565590=storedRootEntryMean

		assertEqualWithinTolerance("aggMean", aggMean, "storedRootEntryMean", storedRootEntryMean, myEqTolerance)
		// assert(equalWithinTolerance(aggMean, storedRootEntryMean, myEqTolerance), notEqualMsg("aggMean", aggMean, "storedRootEntryMean", storedRootEntryMean, myEqTolerance))
		// s"aggMean=${aggMean} != ${storedRootEntryMean}=storedRootEntryMean, absDiff=${aggMean.-(storedRootEntryMean).abs}")
		val squaredMeanCanned = storedRootEntryMean.pow(2)
		val squaredMeanOrganic = aggMean.pow(2) // == should be same if we use storedMean or sumOfWtdMeans
		assertEqualWithinTolerance("squaredMeanCanned", squaredMeanCanned, "squaredMeanOrganic", squaredMeanOrganic, myEqTolerance)
		//assert(equalWithinTolerance(squaredMeanCanned, squaredMeanOrganic, myEqTolerance))
		val normSqrs = sumOfWtdSqrs./(sumOfMasses)
		// https://stats.stackexchange.com/questions/43159/how-to-calculate-pooled-variance-of-two-or-more-groups-given-known-group-varianc
		val pooledVar = normSqrs.-(squaredMeanOrganic) // TA-DA!!!
		// assert(pooledVar == storedRootEntryVar)
		// JDK9+ has sqrt on BigDecimal. From Scala 2.13 we may have to use Spire or access the Java object, or ...
		// val pooledStdDev = pooledVar.sqrt(mc)
		// val jbd = pooledVar.underlying() // Gets the Java BD
		val aggregateEntry : StatEntry = (firstEntryKey, aggMean, pooledVar )
		aggregateEntry
	}
	def beginCovXprod(binStat : DBinStatClz, eidx : Int, keySyms: IndexedSeq[EntryKey], storedRootMeanVec: IndexedSeq[EntryMean]): BinEntryMidCalc = {

		val covPartnerEntIdx: IndexedSeq[Int] = eidx + 1 to keySyms.size - 1
		val vwt = binStat.massInfo.binMass
		val statRow = binStat.statRow
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
		(vwt, localEntry, covShortRow)
	}
	// type VwtCovTup = (EntryKey, EntryKey, VwtCov)
	// type VwtCovRow = IndexedSeq[VwtCovTup]
	// wtEntStatTups tells the covariance row for each bins.  We do the weighted sum of those covariance rows.
	// TODO:  Derive the emptyShortRowWtCov from wtEntStatTups.head
	def finishShortCovRow(wtEntStatTups: IndexedSeq[(VagueWt, StatEntry, UnwtCovRow)], emptyShortRowWtCov : VwtCovRow): VwtCovRow = {
		// Total up the input rows to produce a single row of covariances - all the covariances for entry.
		// val emptyShortRowWtCov : VwtCovRow = covPartnerEntIdx.map(cpeidx => (ekey, keySyms(cpeidx), zeroBD))
		val initTotalMass = myStatEntryOps.zeroBD
		val initResultPair = (emptyShortRowWtCov, initTotalMass)
		val (totalShortCovRow, totalMass) : (VwtCovRow, BinMass) = wtEntStatTups.foldLeft(initResultPair)((prevRsltPair, wtStatTup) => {
			val (prevSumRow, prevWtSum) = prevRsltPair
			val rowWt = wtStatTup._1 // This vwt came from the bin corresponding to this row
			val covShortRow: UnwtCovRow = wtStatTup._3
			val comb: IndexedSeq[(VwtCovTup, UnwtCovPair)] = prevSumRow.zip(covShortRow)
			val summedShortRow : VwtCovRow = comb.map(comboPair => {
				val (prevSumTup, nxtInPair) = comboPair
				val nxtInWeighted = nxtInPair._2.*(rowWt)
				val nxtSum = prevSumTup._3.+(nxtInWeighted)
				(prevSumTup._1, prevSumTup._2, nxtSum)
			})
			val nxtWtSum = prevWtSum.+(rowWt)
			(summedShortRow, nxtWtSum)
		})
		println(s"totalMass=${totalMass}, unnormShortCovRow=${totalShortCovRow}")
		val aggShortCovRow = totalShortCovRow.map(covTup => {
			val covSum = covTup._3
			val aggCov = covSum./(totalMass)
			(covTup._1, covTup._2, aggCov)
		})
		println(s"aggShortCovRow=${aggShortCovRow}")
		aggShortCovRow
	}
}
trait TooManyTypes {
	type PairFunc[L,R]=Function0[(L,R)]
	type WeirdFunc[A,B,C,D] = (A,D) => (B,C)
	// type RPair=(String,Iterable[])

}