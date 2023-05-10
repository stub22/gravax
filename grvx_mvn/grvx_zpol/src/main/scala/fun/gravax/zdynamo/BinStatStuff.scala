package fun.gravax.zdynamo

// import fun.gravax.zdynamo.RunZioDynamoTrial.{BinStoreRslt, DBinWt, LevelTagNumChnk, ParentTag, StatRow, combineWeightMeansAndVars, myGenBD, myBinStatCalc, myBinSumCalc}
import zio.{Chunk, NonEmptyChunk, UIO}
import zio.stream.{UStream, ZStream}

private trait BinStatStuff

trait BinSummaryCalc extends KnowsGenTypes  {

	val myBinStatCalcs = new BinStatCalcs {}
	// type LevelTagNumChnk = NonEmptyChunk[(BinTagInfo, BinNumInfo)]
	def combineStatsPerParent(storeRsltChnk : Chunk[BinStoreRslt], parentTagNums : LevelTagNumChnk) : UIO[Chunk[(BinTagInfo, DBinWt, StatRow)]] = {

		// Results in storeRsltChnk are already sorted by parents, and should be in same order as parents in fullBinTagNumBlk
		val rsltChnksPerParent: UStream[(ParentTag, NonEmptyChunk[BinStoreRslt])] =
			ZStream.fromChunk(storeRsltChnk).groupAdjacentBy(rslt => rslt._1._1.parentTag)
		val chunkOfChunksOp: UIO[Chunk[(ParentTag, NonEmptyChunk[BinStoreRslt])]] = rsltChnksPerParent.runCollect
		val aggUIO: UIO[Chunk[(BinTagInfo, DBinWt, StatRow)]] = chunkOfChunksOp.map(cofc => {
			// If all of the parents have at least one child, then parentTagNums and cofc should be same length and in same order.
			assert(cofc.size == parentTagNums.size)
			// If inputs are NOT of equal size, the returned chunk will have the length of the shorter chunk.
			// : Chunk[(BinTagInfo, BinNumInfo, (ParentTag, NonEmptyChunk[BinStoreRslt]))]
			val combo = parentTagNums.toChunk.zip(cofc)
			assert(combo.size == parentTagNums.size)
			// Could make this a stream to
			// combo.foreach(row =>
			val aggregateRowChunk: Chunk[(BinTagInfo, DBinWt, StatRow)] = combo.map(row => {
				val (tagInfo, numInfo, (ptag, rsltChnk)) = row
				// Ensure that the parent binTags match up as expected.
				assert(tagInfo.binTag == ptag)
				val (aggWt, aggStatRow) = combineWeightMeansAndVars(rsltChnk)
				val outTup = (tagInfo, aggWt, aggStatRow)
				outTup
			})
			aggregateRowChunk
		})
		aggUIO
	}
	def combineWeightMeansAndVars(baseRsltSeq : IndexedSeq[BinStoreRslt]) : (DBinWt, StatRow) = {
		val binDatChunk : IndexedSeq[BinTypes.DBinDat] = baseGenRsltsToDBinDats(baseRsltSeq)
		myBinStatCalcs.aggregateWeightsMeansAndVars(binDatChunk)
	}
	private def baseGenRsltsToDBinDats(baseRsltSeq : IndexedSeq[BinStoreRslt]) : IndexedSeq[DBinDat] = {
		val (taggedDBDs, ekeys) = baseGenRsltsToTaggedDBinDatsAndEKeys(baseRsltSeq)
		taggedDBDs.map(_._3)
	}
	private def baseGenRsltsToTaggedDBinDatsAndEKeys(baseRsltSeq : IndexedSeq[BinStoreRslt]) : (IndexedSeq[(ParentTag, BinTag, DBinDat)], IndexedSeq[BinTypes.EntryKey]) = {
		val binSpecs = baseRsltSeq.map(_._1)
		val firstMeat = binSpecs.head._4
		val meatKeyOrder = Ordering.String
		val keySeq : IndexedSeq[BinTypes.EntryKey] = firstMeat.allKeysSorted(meatKeyOrder)
		val binDatSeq : IndexedSeq[(ParentTag, BinTag, DBinDat)] = binSpecs.map(binSpec => {
			val dbd : DBinDat = binSpecToDBD(binSpec, keySeq)
			val tagInfo : BinTagInfo = binSpec._1
			(tagInfo.parentTag, tagInfo.binTag, dbd)
		})
		(binDatSeq, keySeq)
	}
	private def binSpecToDBD(bbSpec : BinSpec, keySyms: IndexedSeq[EntryKey]) : DBinDat = {
		val (tagInfo, numInfo, massInfo, binMeat) = bbSpec
		val statRow = binMeat.mkStatRow(keySyms)
		val binIdHmm = -999 // tagInfo.binTag
		val dbd = (binIdHmm, massInfo.binMass, statRow )
		dbd
	}
/*
	def computeStatsForParents(baseRsltSeq : IndexedSeq[BinStoreRslt]) : IndexedSeq[(ParentTag, StatRow)] = {
		val (taggedDBDs, keySeq) = gbd.baseGenRsltsToTaggedDBinDatsAndEKeys(baseRsltSeq)
		val parentStats : IndexedSeq[(ParentTag, StatRow)] = betterParentStats(taggedDBDs, keySeq)
		parentStats
	}
	def betterParentStats(binDataMatrix : IndexedSeq[(ParentTag, BinTag, DBinDat)], keySyms: IndexedSeq[EntryKey]): IndexedSeq[(ParentTag, StatRow)] = {
		???
	}
*/
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