package fun.gravax.distrib.struct

import fun.gravax.distrib.calc.{DBinStatClz, KnowsDistribTypes}
import zio.{Task, ZIO}

import scala.collection.immutable.{Map => SMap}

private trait BinDataStuff

trait KnowsDistKeyShapes {
	type DistScenario = String
	type DistTime = String
}
trait KnowsBinTagShapes {
	type BinTag = String
	type ParentTag = BinTag
	type BinFlavor = String
}
trait KnowsStatTupleShapes extends KnowsBinTagShapes with KnowsDistKeyShapes {


	// Building up data-types this way (vs. by traits) is ... extensional and sorta constructivist / algebraic.
	type EntryKey = String
	type EntryValue = BigDecimal

	type EntryMean = BigDecimal
	type ExpectSquare = BigDecimal
	type EntryVar = BigDecimal	// Often this is marginal variance
	type StatEntry = (EntryKey, EntryMean, EntryVar)
	type StatRow = IndexedSeq[StatEntry]   // For some set of keySyms, in some useful order that is not specified by type.
	type StatMap = SMap[EntryKey, StatEntry]

	type DBinMass = BigDecimal
	type DBinRelWt = BigDecimal
	type DBinAbsWt = BigDecimal
	type VagueWt = BigDecimal	// VagueWt is usable in calcs where mass-vs-wt doesn't matter.

	type EntryExpects = (EntryKey, EntryMean, ExpectSquare)
	type VwtExpectsRow = (VagueWt, IndexedSeq[EntryExpects])

	type DBinID = BinTag

	// One advantage of using class (vs. tup) is we can search the code for usages of individual fields.
	// type DBinDatTup = (DBinID, DBinRelWt, StatRow) // Does NOT contain covariances.
	// type DBinDat = DBinDatTup
	// type DBinMatrix = IndexedSeq[DBinDat]

	type DBinStatMatrix = IndexedSeq[DBinStatClz]

	// Weighted expected squared value and mean
	type VwtdSqrAndMean = (BigDecimal, BigDecimal)

	type KindaCrazy[X[_]] = List[X[String]]



}

object BinTypes extends KnowsStatTupleShapes



// Generally we don't store Covariances in bins.
// Note that bins may be wide (100s of assets) and full covariance takes order-squared space.
// Instead we compute covariance for a selection of assets, on the fly.
trait BinData extends KnowsStatTupleShapes {
	def getScenarioID : DistScenario
	def getObsTime : DistTime
	def getPredTime : DistTime
	def getCalcTime : DistTime
	def getBinTagTxt : BinTag
	def getParentTagTxt : BinTag
	def getBinFlavor : BinFlavor // TODO: Make enum-ish
	// def getRelWt : DBinRelWt
	// def getAbsWt : BigDecimal
	def getMass : DBinRelWt

	// protected def getStatMap : StatMap

	// Called from BinNode
	def mkStatRow(keySeq : IndexedSeq[EntryKey]) : Task[StatRow]

	def getDBSC(keySeq : IndexedSeq[EntryKey]) : Task[DBinStatClz]


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

	override def getObsTime: DistTime = myTimeInfo.obsTime

	override def getPredTime: DistTime = myTimeInfo.predTime

	override def getCalcTime: DistTime = myTimeInfo.calcTime

	override def getBinTagTxt: BinTag = myTagInfo.binTag

	override def getParentTagTxt: BinTag = myTagInfo.parentTag

	override def getBinFlavor: BinFlavor = myTagInfo.binFlavor

	override def getMass: DBinRelWt = myMassInfo.binMass

	// override def getRelWt: DBinRelWt = myMassInfo.relWt_opt.get

	override def mkStatRow(keySeq: IndexedSeq[EntryKey]): Task[StatRow] = getMeatInfoOp.map(_.mkStatRow(keySeq))

	override def getDBSC(keySeq: IndexedSeq[EntryKey]): Task[DBinStatClz] = {
		val statRowTask = mkStatRow(keySeq)
		statRowTask.map(statRow => {
			DBinStatClz(myTagInfo, myMassInfo, statRow)
		})
	}

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
