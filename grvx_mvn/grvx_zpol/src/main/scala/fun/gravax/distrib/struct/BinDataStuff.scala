package fun.gravax.distrib.struct

import fun.gravax.distrib.calc.KnowsDistribTypes

import scala.collection.immutable.{Map => SMap}

private trait BinDataStuff

trait StatTupleShapes {
	// Building up data-types this way (vs. by traits) is ... extensional and sorta constructivist / algebraic.
	type EntryKey = String
	type EntryValue = BigDecimal

	type EntryMean = BigDecimal
	type ExpectSquare = BigDecimal
	type EntryVar = BigDecimal	// Often this is marginal variance
	type StatEntry = (EntryKey, EntryMean, EntryVar)
	type StatRow = IndexedSeq[StatEntry]   // For some set of keySyms, in some useful order that is not specified by type.
	type StatMap = SMap[EntryKey, StatEntry]

	type EntryExpects = (EntryKey, EntryMean, ExpectSquare)
	type WtExpectsRow = (DBinWt, IndexedSeq[EntryExpects])

	type DBinID = Int
	type DBinWt = BigDecimal
	type DBinDat = (DBinID, DBinWt, StatRow) // Does NOT contain covariances.

	type DBinMatrix = IndexedSeq[DBinDat]

	// Weighted expected squared value and mean
	type WtdSqrAndMean = (BigDecimal, BigDecimal)

	type KindaCrazy[X[_]] = List[X[String]]

	type BinTag = String
	type ParentTag = BinTag

	type BinFlavor = String
}

object BinTypes extends StatTupleShapes

// Generally we don't store Covariances in bins.
// Note that bins may be wide (100s of assets) and full covariance takes order-squared space.
// Instead we compute covariance for a selection of assets, on the fly.
trait BinData extends StatTupleShapes {
	def getScenarioID : String
	def getObsTime : String
	def getPredTime : String
	def getCalcTime : String
	def getBinTagTxt : String
	def getBinNumInt : Int
	def getParentTagTxt : String
	def getParentNumInt : Int
	def getBinFlavor : String // TODO: Make enum-ish
	def getRelWt : BigDecimal
	// def getAbsWt : BigDecimal
	def getMass : BigDecimal

	def getStatMap : StatMap

	def mkStatRow(keySeq : IndexedSeq[EntryKey]) : StatRow

	def allKeysSorted(meatKeyOrder : Ordering[String]) : IndexedSeq[EntryKey]

}

case class BinKeyInfo(scenPartKey : String, compoundSortKey : String)

case class BinTimeInfo(obsTime : String, predTime : String, calcTime : String)
case class BinTagInfo(binTag : String, parentTag : String) // levelNum, siblingNum
case class BinMassInfo(binMass : BigDecimal, relWt_opt : Option[BigDecimal], absWt_opt : Option[BigDecimal] = None)



// TODO: add these index numbers to persistent store?
//
case class BinNumInfo(binNum : Int, parentNum : Int, maxKids : Int, levelNum : Int, siblingNum : Int)

// Seems we cannot use abstract types (of our self-type, or inherited) in constructor parameters.
// If we make an outer trait scope then those names are available, or we can refer to members of an object.
case class BinMeatInfo(binFlavor : BinTypes.BinFlavor, meatMap : BinTypes.StatMap) extends KnowsDistribTypes {
	def mkStatRow(keySeq : IndexedSeq[EntryKey]) : StatRow = {
		val entrySeq : StatRow = keySeq.map(ksym => {
			val statAtSym: StatEntry = meatMap.get(ksym).get // This second .get will throw if no value present
			statAtSym
		}).toIndexedSeq
		entrySeq
	}
	def allKeysSorted(meatKeyOrder : Ordering[String]) : IndexedSeq[EntryKey] = {
		val keySet = meatMap.keySet
		keySet.toSeq.sorted(meatKeyOrder).toIndexedSeq
	}
}

case class EzBinData(scenID : String, timeDat : BinTimeInfo, seqDat : BinTagInfo, massDat : BinMassInfo, meat : BinMeatInfo) extends BinData {
	override def getScenarioID: String = scenID
	override def getObsTime: String = timeDat.obsTime
	override def getPredTime: String = timeDat.predTime
	override def getCalcTime: String = timeDat.calcTime
	override def getBinTagTxt: String = seqDat.binTag
	override def getBinNumInt: Int = ???
	override def getParentTagTxt: String = seqDat.parentTag
	override def getParentNumInt: Int = ???
	override def getMass: BigDecimal = massDat.binMass
	override def getRelWt: BigDecimal = massDat.relWt_opt.get
	// override def getAbsWt: BigDecimal = massDat.absWt
	override def getBinFlavor: String = meat.binFlavor
	override def getStatMap: StatMap = meat.meatMap

	override def mkStatRow(keySeq: IndexedSeq[EntryKey]): StatRow = meat.mkStatRow(keySeq)
	override def allKeysSorted(meatKeyOrder : Ordering[String]) : IndexedSeq[EntryKey] = meat.allKeysSorted(meatKeyOrder)
}

case class BinNode(myDat : BinData, parent_opt : Option[BinNode], myKids : Iterable[BinNode], meatKeyOrder : Ordering[String])  extends VecDistribFragment  {

	// Assume meatKeys are same across all bins
	// override def getFullKeySymSet : Set[EntryKey] = myDat.getStatMap.keySet

	// Projects data from the main myDat layer of this BinNode, for given subset of syms.  No info from the myKids is used.
	// Will throw on failed lookup
	override def projectStatRow(keySyms: IndexedSeq[EntryKey]): StatRow = myDat.mkStatRow(keySyms)

	// Useful?  This just repackages the info from myDat, with the keys in our given ordering.
	lazy val myFullBinDat : DBinDat = {
		val keysInOrder = myDat.allKeysSorted(meatKeyOrder)
		projectToDBD(keysInOrder)
	}

	def projectToDBD (orderedSyms : IndexedSeq[EntryKey]) : DBinDat = {
		val projStatRow = projectStatRow(orderedSyms)
		(myDat.getBinNumInt, myDat.getRelWt, projStatRow)
	}

	// TODO:  We often (or...always?) want to force all children to have subtrees of equal queryDepth.
	def getMaxDepth : Int = ???

	// Collect all bins at the "queryDepth" level (not from any other levels!) into a single matrix, whose weights
	// should sum to 1.0.
	// This is the complete dataset for the depth-order (as in "nth-order") approximation to the distribution.
	// TODO: Add a stream-oriented version of this algo.
	def projectAndCollectBins(orderedSyms : IndexedSeq[EntryKey], queryDepth : Int) : DBinMatrix = {
		// TODO:  Check queryDepth <= maxDepth, else throw
		if ((queryDepth == 0) || myKids.isEmpty) {
			val onlyOneBin = projectToDBD(orderedSyms)
			IndexedSeq(onlyOneBin)
		} else {
			// Recursively descend until we reach queryDepth.  Only bins from that depth will be aggregated here,
			// except when subtrees don't extend that far we will get the interim summary bins (the .isEmpty case above)
			val bmtrx: Iterable[DBinDat] = myKids.flatMap(childNode =>
				childNode.projectAndCollectBins(orderedSyms, queryDepth - 1)
			)
			bmtrx.toIndexedSeq
		}
	}
}


/*
chk

import java.time.{Instant => JInstant}

case class PredictionTimeInfo(obsTime : JInstant, calcTime : JInstant, endTime : JInstant)

trait BinModel {

	type QV // Quantitative value that acts like a Vector, allows computation of means.
	type ScenarioID = String

	type TimeInfo = PredictionTimeInfo

	case class BinRecord(scenario : ScenarioID, myTestTimeInf : TimeInfo, mean : QV, count_opt: Option[Int], weight_opt: Option[BigDecimal])

	def makeZDI(br : BinRecord) : Item
}

class MultiAssetBinModel extends BinModel {
	type AssetID = String

	type Amount = BigDecimal // M

	override type QV = SMap[String, Amount]

	override def makeZDI(br: BinRecord): Item = ???
}
*/
