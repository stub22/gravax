package fun.gravax.zdynamo

import zio.dynamodb.Item

import scala.collection.immutable.{Map => SMap}

private trait BinDataStuff

trait BinData {
	def getScenarioID : String
	def getObsTime : String
	def getPredTime : String
	def getCalcTime : String
	def getBinSeq : String
	def getParentBinSeq : String
	def getBinFlavor : String // TODO: Make enum-ish
	def getRelWt : BigDecimal
	def getAbsWt : BigDecimal
	def getMass : BigDecimal
	def getAnnRetMeans : Map[String, BigDecimal]
}
case class BinTimeData(obsTime : String, predTime : String, calcTime : String)
case class BinSeqInfo(binSeq : String, parentBinSeq : String)
case class BinMassInfo(binMass : BigDecimal, relWt : BigDecimal, absWt : BigDecimal)
case class BinMeat(binFlavor : String, meatMap : SMap[String, BigDecimal])
case class EzBinData(scenID : String, timeDat : BinTimeData, seqDat : BinSeqInfo, massDat : BinMassInfo, meat : BinMeat) extends BinData {
	override def getScenarioID: String = scenID
	override def getObsTime: String = timeDat.obsTime
	override def getPredTime: String = timeDat.predTime
	override def getCalcTime: String = timeDat.calcTime
	override def getBinSeq: String = seqDat.binSeq
	override def getParentBinSeq: String = seqDat.parentBinSeq
	override def getMass: BigDecimal = massDat.binMass
	override def getRelWt: BigDecimal = massDat.relWt
	override def getAbsWt: BigDecimal = massDat.absWt
	override def getBinFlavor: String = meat.binFlavor
	override def getAnnRetMeans: SMap[String, BigDecimal] = meat.meatMap
}


import java.time.{Instant => JInstant}

case class PredictionTimeInfo(obsTime : JInstant, calcTime : JInstant, endTime : JInstant)

trait BinModel {

	type QV // Quantitative value that acts like a Vector, allows computation of means.
	type ScenarioID = String

	type TimeInfo = PredictionTimeInfo

	case class BinRecord(scenario : ScenarioID, timeInf : TimeInfo, mean : QV, count_opt: Option[Int], weight_opt: Option[BigDecimal])

	def makeZDI(br : BinRecord) : Item
}

class MultiAssetBinModel extends BinModel {
	type AssetID = String

	type Amount = BigDecimal // M

	override type QV = SMap[String, Amount]

	override def makeZDI(br: BinRecord): Item = ???
}