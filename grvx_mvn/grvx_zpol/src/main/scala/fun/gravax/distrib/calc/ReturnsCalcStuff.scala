package fun.gravax.distrib.calc

import fun.gravax.distrib.struct.{KnowsStatTupleShapes, VecDistrib}
import zio.Task

private trait ReturnsCalcStuff


trait HoldingOptimizer extends KnowsStatTupleShapes {
	type	AssetID = EntryKey
	type 	MultiAssetAnnRetDist = VecDistrib
	type	AnnRet = EntryMean
	type 	AnnRetVar = EntryVar
	type	AssetRet = StatEntry //  (AssetID, AnnRet, AnnRetVar) // Same as StatEntry
	type	HoldWt = (AssetID, BigDecimal)

	type	AnnRetDist

	type	SharpeRatio = BigDecimal

	type OptimResult =  (Map[AssetID, BigDecimal], AnnRetDist, SharpeRatio)

	// Could use gradient descent or another method.
	// Client could pass in a Seq[AssetID] in order to control ordering in the output.
	def optimizeSharpe(availAssets : Set[AssetID], maRetDist : MultiAssetAnnRetDist) : OptimResult = {
		val availAssetSeq = availAssets.toIndexedSeq
		// Expected portfolio return will be dot-product of our portfolio asset weights with these mean asset returns.
		// This is a purely linear calculation, with a single number as result.
		val rootBinStat_op: Task[DBinStatClz] = maRetDist.projectRootBin(availAssetSeq)
		rootBinStat_op.map(rootStat => {

			// val (rootBinID, rootBinWt, rootBinRow) = rootDBD
			val rootMass = rootStat.massInfo.binMass
			val rootBinTag = rootStat.tagInfo.binTag
			val bd1 = BigDecimal("1.0")
			// assert (rootBinWt == bd1)
			// Computing the portfolio variance is more complicated.  Each child bin represents some PART of the total
			// distribution (but the support of the bins may be overlapping).  For a given set of portfolio asset weights,
			// the variance will be the expected squared difference over all possible outcomes.  This can be treated as
			// the weighted sum of the variance found in each bin (computed relative to the GLOBAL mean of the distribution).
			// Using only the mean return of the bin as a representative gives us an imperfect estimate of the
			// bin-variance-relative-to-global, in which we skip over the squared-value integral.
			// If the bins are chosen to be heavily overlapping, then our estimated variance will tend to undershoot.
			// Our method is more accurate when the bins are mostly disjoint.  We can try to improve by asking for
			// additional information with the bins.
			// Note that Sortino ratio emphasizes the downside variance, whereas Sharpe treats all variance equally.
			val childStatsOp : Task[IndexedSeq[DBinStatClz]] = maRetDist.projectChildBins(rootBinTag, availAssetSeq)
			// Now we need either
			// 1) An iterative algorithm, e.g. gradient descent.
			// 2) A gaussian approximation using covariances (which turns optimization into a matrix-inverse problem).
			// 3) An optimization expression that we can differentiate and solve for 0 to find candidate extrema.
			// 4) A monte-carlo guesser.  This might be the easiest way to start.
			// In some scenarios we may allow negative weights.
			val distsFromMean = ??? // childDBDs.map()
			???
		})
		???
	}


	def calcVariance(meanRet : AnnRet, assetWeights : Seq[HoldWt], binStats : Seq[DBinStatClz])

	// TODO:  Make this expression differentiable in the asset weights, so that we can use gradient descent.
	def estimVariance(meanRet : AnnRet, assetWeights : Seq[HoldWt], binDats : Seq[DBinStatClz]) : AnnRetVar = {
		val weightedSquaredDevs : Seq[AnnRetVar] = binDats.map(binStat => {
			val binRow: StatRow = binStat.statRow // binDat._3
			val binARs : Seq[AssetRet] = binRow
			val estimRetForBin = calcReturnAtVec(assetWeights, binARs)
			val estimDiff = estimRetForBin - meanRet
			val estimDiffSq = estimDiff.pow(2)  // This is where the estimation goes a little wobbly.
			// ...because integral of squared diff (over the whole bin) is not the same as square of mean diff of the bin.
			// For example, if the bin contains very-large-diff outliers they will be under-represented in this variance.
			// Maybe if variance is >= 1 this estim turns out to be a lower bound, but if var < 1 it's an upper bound?
			// However if the client proceeds to use additional layers of the BinDat it can get a more precise estimate.
			val binVwt: DBinRelWt = binStat.massInfo.binMass // binDat._2
			val weightedEDSq = binVwt.*(estimDiffSq)
			weightedEDSq
		})
		val summedWSDs = weightedSquaredDevs.reduce((ltSum, rtSum) => ltSum.+(rtSum))
		summedWSDs
	}
	def calcReturnAtVec(assetWeights : Seq[HoldWt], assetReturns : Seq[AssetRet]) : AnnRet = {
		assert(assetWeights.length == assetReturns.length)
		val paired = assetWeights.zip(assetReturns)
		val totRet : AnnRet = paired.foldLeft(BigDecimal("0.0"))((sum, pair) => {
			val (holdWt, assetRet) = pair
			assert(holdWt._1 == assetRet._1)
			sum.+(holdWt._2.*(assetRet._2))
		})
		totRet
	}
}