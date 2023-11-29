package fun.gravax.dbin.model

import fun.gravax.dbin.model.CovarTypes.{EEProduct, EntryCovariance}
import fun.gravax.dbin.model.DbinModelTypes.{BinRelWt, EntryKey, EntryMean, EntryVariance}
import fun.gravax.dbin.model.PortfolioTypes.{AssetWeight, ExpPortfSquare, PortfMean, PortfSquaredMean, PortfVar}

class CovarModel {

}

object CovarTypes {
	type EntryCovariance = BigDecimal
	type EEProduct = BigDecimal
}

// If we assume that CovarMatrix "has access" to the required data, then we probably wind up relying on a cache.
// If we want to get benefits from streaming the bins, we have to orchestrate a calculation scenario that is
// not strictly on-demand.  These matrices are sink-like destinations.  They capture a currently interesting
// subset of a Distrib in a reasonably dense form, in memory, as the accumulated result of a streamed calc.

/*
We need to clarify the scenarios that the Covar is needed for, so we know 'who is asking' for Covars?

One answer is that we need covars to optimize a portfolio in traditional style (efficient horizon).

In principle a CovarMatrix can be computed *or* stored, or even input directly as a form of model, possibly
using some hypothetical assets in addition to real ones.

We may think of a matrix as a function that produces values.  Then the data of the matrix is subsumed into
the mapping-rule of the function.
*/

trait CovarMatrix {
	def getCovarOrThrow(e1 : EntryKey, e2 : EntryKey) : EntryCovariance
	def getKeys : Seq[EntryKey]
}

trait PairwiseProductMatrix {
	def getExpectedProductOrThrow(e1: EntryKey, e2: EntryKey): EEProduct

	def getKeys: Seq[EntryKey]
}

trait BinCovarOps {

	def estimCovarMatrix(entryKeys: Seq[EntryKey])(parentBin: Bin, numLevels: Int): CovarMatrix = {
		// A) Get the pairwiseProductMatrix from the appropriate child level.
		// B) Get the global means from parentBin, and make a pairwise matrix of products from that.
		// Subtract matrix B from A.
		???
	}


	def estimPairwiseProducts(entryKeys : Seq[EntryKey])(parentBin : Bin, numLevels : Int) : PairwiseProductMatrix = {
		if (numLevels == 0) {
			// Just multiply out the expected values in parentBin
			???
		} else {
			// Access some bin source to get the bottom-level bins, paired with their weights relative to parentBin.
			// Get all their pairwise products, and make their weighted sum.
			// This can work as a recursive fetch, or we can simply jump to the bottom-level, as long as we have
			// access to the necessary weighting information.
			???
		}
	}

	def expSquareForOneEntryInOneBin(entryKey : EntryKey, bin: Bin) = ???

	def expProductInBin(bin : Bin, subLevels : Int)(ekA : EntryKey, ekB : EntryKey) : EEProduct = {
		// If subLevels is 0, then (for A != B) exp(A*B) == exp(A) * exp(B).  Right?
		// But for A==B, we have exp(A*A) = exp(A)^2 + var(A)
		// As alternative to subLevels, we might pass in the local covariance estimate.
		// Should Bin carry an optional covariance estimate?
		???
	}
}

object PortfolioTypes {
	type AssetWeight = BigDecimal
	type PortfMean = BigDecimal
	type PortfVar = BigDecimal
	type PortfSquaredMean = BigDecimal
	type ExpPortfSquare = BigDecimal
}
trait PortfolioOps {

	// A general assumption here is that our portfolio numbers (which are usually some form of predicted return)
	// must be summable.
	// So additive returns like 0.05, 0.00, -0.05 make sense, but not multiplicative ones like 0.95, 1.05.

	// pweights and binWeights MIGHT be assumed to be Convex (i.e. sum to 1.0).
	// Let's note exactly when this assump needs to be applied.


	// Here we don't assume that a CovarMatrix is already available.
	def estimPortfolioVariance(pweights: Map[EntryKey, AssetWeight], globalMeans: Map[EntryKey, EntryMean],
							   weightedBins: Seq[(BinRelWt, Bin)]): EntryVariance = ???


	/* In this version, we do require a pre-existing covarMatrix.  Of course it *could* be lazy.
	* But if it needed to load binData from some BinSource, that would usually require an effect.
	
	We are leaning towards paying the price of abstraction over the nature of the BinSource.
	Is it an in-memory cache backed by a DB Table?  Or is it somehow going to be streamed later in an optimized job?
	*/
	def estimPortfolioVariance(pweights: Map[EntryKey, AssetWeight], covarMatrix : CovarMatrix): PortfVar = {
		// https://en.wikipedia.org/wiki/Covariance#Covariance_of_linear_combinations
		val ekvs: Seq[(EntryKey, AssetWeight)] = pweights.toSeq
		val ekvtmps: Seq[((EntryKey, AssetWeight), (EntryKey, AssetWeight))] = triangularMatrixPairs(ekvs)
		val terms: Seq[BigDecimal] = ekvtmps.map((pairA, pairB) => {
			val (ekA, awA) = pairA
			val (ekB, awB) = pairB
			// So far we assume that .getCovar can be done without any effects, but maybe this is a spot where we could
			// benefit by using a generic effect-wrapper, defaulting to Identity or Eval.
			val covAB: EntryCovariance = covarMatrix.getCovarOrThrow(ekA, ekB)
			// Terms on the diagonal are self-variance
			// Terms off the diagonal are co-variance and need to be counted twice
			val dblr : BigDecimal = if (ekA == ekB) BigDecimal(1) else BigDecimal(2)
			val termAB = dblr.*(awA).*(awB).*(covAB)
			termAB
		})
		val sum: PortfVar = terms.sum // Relies on summability of the separate returns
		sum
		// , globalMeans: Map[EntryKey, EntryMean],
		// val pMean : PortfMean = ???
		//val pMeanSq : PortfSquaredMean = pMean.pow(2)
		// val expPSq : ExpPortfSquare = {
			// val ekeys: Seq[EntryKey] = pweights.keys.toSeq
			// val ekeyTMPs: Seq[(EntryKey, EntryKey)] = triangularMatrixPairs(ekeys)
			// ???
	//	}
		// val pVar = expPSq.-(pMeanSq)
		// pVar
	}

	def estimPortfolioMean(pweights: Map[EntryKey, AssetWeight], globalMeans: Map[EntryKey, EntryMean]) : PortfMean = {
		// Easy - just take the dot-product:  Multiply each mean by appropriate weight, and sum.
		???
	}

	def triangularMatrixPairs[A](inSeq : Seq[A]) : Seq[(A,A)] = ???


}

class PortfOpsImpl extends PortfolioOps {
	override def estimPortfolioVariance(pweights: Map[EntryKey, AssetWeight], globalMeans: Map[EntryKey, EntryMean],
										weightedBins: Seq[(BinRelWt, Bin)]): EntryVariance = {
		???
	}
}