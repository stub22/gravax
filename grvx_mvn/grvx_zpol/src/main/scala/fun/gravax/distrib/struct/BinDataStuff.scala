package fun.gravax.distrib.struct

import fun.gravax.distrib.calc.KnowsDistribTypes
import zio.{Task, ZIO}

import scala.collection.immutable.{Map => SMap}

private trait BinDataStuff

trait KnowsStatTupleShapes {
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

object BinTypes extends KnowsStatTupleShapes

// Generally we don't store Covariances in bins.
// Note that bins may be wide (100s of assets) and full covariance takes order-squared space.
// Instead we compute covariance for a selection of assets, on the fly.
trait BinData extends KnowsStatTupleShapes {
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

	// protected def getStatMap : StatMap

	// Called from BinNode
	def mkStatRow(keySeq : IndexedSeq[EntryKey]) : Task[StatRow]

	// Called from BinNode, but only by unused val  myFullBinDat
	def allKeysSorted(meatKeyOrder : Ordering[String]) : Task[Seq[EntryKey]]

}

trait BinDataUsingInfo extends BinData {
	protected def getTimeInfo : BinTimeInfo
	private lazy val myTimeInfo = getTimeInfo

	protected def getTagInfo : BinTagInfo
	private lazy val myTagInfo = getTagInfo

	protected def getMassInfo : BinMassInfo
	private lazy val myMassInfo = getMassInfo

	protected def getMeatInfoOp :  Task[BinMeatInfo]

	// override def getScenarioID: BinFlavor = ???

	override def getObsTime: BinFlavor = myTimeInfo.obsTime

	override def getPredTime: BinFlavor = myTimeInfo.predTime

	override def getCalcTime: BinFlavor = myTimeInfo.calcTime

	override def getBinTagTxt: BinFlavor = myTagInfo.binTag

	override def getBinNumInt: DBinID = ???

	override def getParentTagTxt: BinFlavor = myTagInfo.parentTag

	override def getParentNumInt: DBinID = ???

	override def getBinFlavor: BinFlavor = myTagInfo.binFlavor

	override def getMass: DBinWt = myMassInfo.binMass

	override def getRelWt: DBinWt = myMassInfo.relWt_opt.get

	override def mkStatRow(keySeq: IndexedSeq[EntryKey]): Task[StatRow] = getMeatInfoOp.map(_.mkStatRow(keySeq))

	override def allKeysSorted(meatKeyOrder : Ordering[EntryKey]) : Task[Seq[EntryKey]] = getMeatInfoOp.map(_.allKeysSorted(meatKeyOrder))

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
