package fun.gravax.dbin.model

import fun.gravax.dbin.model.CovarTypes.EntryCovariance
import fun.gravax.dbin.model.DbinModelTypes.{BinRelWt, EntryKey, EntryMean, EntryVariance}
import fun.gravax.dbin.model.PortfolioTypes.{AssetWeight, ExpPortfSquare, PortfMean, PortfVar, PortfSquaredMean}

class CovarModel {

}

object CovarTypes {
	type EntryCovariance = BigDecimal
}

// If we assume that CovarMatrix "has access" to the required data, then we are thinking in OOP style.

trait CovarMatrix {
	def getCovarOrThrow(e1 : EntryKey, e2 : EntryKey) : EntryCovariance
	def getKeys : Seq[EntryKey]
}

trait BinCovarOps {
	def estimCovars(globalMeans : Map[EntryKey, EntryMean], weightedBins: Seq[(BinRelWt, Bin)]) : CovarMatrix


}

object PortfolioTypes {
	type AssetWeight = BigDecimal
	type PortfMean = BigDecimal
	type PortfVar = BigDecimal
	type PortfSquaredMean = BigDecimal
	type ExpPortfSquare = BigDecimal
}
trait PortfolioOps {

	def estimPortfolioVariance(pweights: Map[EntryKey, AssetWeight], globalMeans: Map[EntryKey, EntryMean],
							   weightedBins: Seq[(BinRelWt, Bin)]): EntryVariance

	// pweights and binWeights may be assumed to be Convex (i.e. sum to 1.0).
	// Let's note exactly when this assump needs to be applied.
	def estimPortfolioVariance(pweights: Map[EntryKey, AssetWeight], globalMeans: Map[EntryKey, EntryMean],
							   covarMatrix : CovarMatrix): PortfVar = {
		val pMean : PortfMean = ???
		val pMeanSq : PortfSquaredMean = pMean.pow(2)
		val expPSq : ExpPortfSquare = {
			val ekeys = pweights.keys
			???
		}
		val pVar = expPSq.-(pMeanSq)
		pVar
	}

	def estimPortfolioMean(pweights: Map[EntryKey, AssetWeight], globalMeans: Map[EntryKey, EntryMean]) : PortfMean = {
		// Easy - just take the dot-product:  Multiply each mean by appropriate weight, and sum.
		???
	}


}

class PortfOpsImpl extends PortfolioOps {
	override def estimPortfolioVariance(pweights: Map[EntryKey, AssetWeight], globalMeans: Map[EntryKey, EntryMean],
										weightedBins: Seq[(BinRelWt, Bin)]): EntryVariance = {
		???
	}
}