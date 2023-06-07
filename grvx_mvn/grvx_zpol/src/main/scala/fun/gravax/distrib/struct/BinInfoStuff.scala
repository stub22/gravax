package fun.gravax.distrib.struct

import fun.gravax.distrib.binstore.KnowsBinItem
import fun.gravax.distrib.calc.{DBinStatClz, KnowsDistribTypes}
import fun.gravax.distrib.gen.KnowsBinTupTupTypes
import zio.{IO, Task, ZIO}
import zio.dynamodb.PrimaryKey

private trait BinInfoStuff

case class BinFullKeyInfo(tblNm : String, scenPartKey : String, compoundSortKey : String) extends KnowsBinItem  {
	def getTableName : String = tblNm
	def getDynamoPK : PrimaryKey = PrimaryKey(KEYNM_PART_SCENARIO -> scenPartKey, KEYNM_SORT_COMPOUND -> compoundSortKey)
}

case class BinTimeInfo(obsTime : String, predTime : String, calcTime : String)
case class BinTagInfo(binTag : BinTypes.BinTag, parentTag : BinTypes.BinTag, binFlavor : BinTypes.BinFlavor) // levelNum, siblingNum
case class BinMassInfo(binMass : BinTypes.BinMass, relWt_opt : Option[BinTypes.BinRelWt], absWt_opt : Option[BinTypes.BinAbsWt] = None)

// TODO: add these index numbers to persistent store?

case class BinNumInfo(binNum : Int, parentNum : Int, maxKids : Int, levelNum : Int, siblingNum : Int)




// Seems we cannot use abstract types (of our self-type, or inherited) in constructor parameters.
// If we make an outer trait scope then those names are available, or we can refer to members of an object.
case class BinMeatInfo(meatMap : BinTypes.StatMap) extends KnowsDistribTypes {
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


abstract class BinDataCore(myScenID : String, myTimeDat : BinTimeInfo, myTagDat : BinTagInfo) extends BinDataUsingInfo {
	override def getScenarioID: String = myScenID
	override protected def getTimeInfo: BinTimeInfo = myTimeDat

	override protected def getTagInfo: BinTagInfo = myTagDat

	override def getBinFlavor: String = myTagDat.binFlavor
}

case class EzBinData(scenID : String, timeDat : BinTimeInfo, myTagInf : BinTagInfo, myMassDat : BinMassInfo, myMeatInf : BinMeatInfo)
		extends BinDataCore(scenID, timeDat, myTagInf) {



	override protected def getMeatInfoOp :  Task[BinMeatInfo] = ZIO.succeed(myMeatInf)

	override protected def getMassInfo: BinMassInfo = myMassDat


}

trait CacheBackedBinData extends BinDataUsingInfo with KnowsBinTupTupTypes {
	protected def getCache : MeatyItemCache
	protected def getBinKey : BinFullKeyInfo
	override protected def getMeatInfoOp :  Task[BinMeatInfo] = {
		val cache = getCache
		val binKey = getBinKey
		cache.get(binKey).map(_.get)
	}
}
