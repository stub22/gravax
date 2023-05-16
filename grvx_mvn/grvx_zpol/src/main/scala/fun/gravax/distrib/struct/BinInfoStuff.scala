package fun.gravax.distrib.struct

import fun.gravax.distrib.binstore.KnowsBinItem
import fun.gravax.distrib.calc.KnowsDistribTypes
import fun.gravax.distrib.gen.KnowsBinTupTupTypes
import zio.IO
import zio.dynamodb.PrimaryKey

private trait BinInfoStuff

case class BinFullKeyInfo(tblNm : String, scenPartKey : String, compoundSortKey : String) extends KnowsBinItem  {
	def getDynamoPK : PrimaryKey = PrimaryKey(KEYNM_PART_SCENARIO -> scenPartKey, KEYNM_SORT_COMPOUND -> compoundSortKey)
}

case class BinTimeInfo(obsTime : String, predTime : String, calcTime : String)
case class BinTagInfo(binTag : String, parentTag : String) // levelNum, siblingNum
case class BinMassInfo(binMass : BigDecimal, relWt_opt : Option[BigDecimal], absWt_opt : Option[BigDecimal] = None)

// TODO: add these index numbers to persistent store?

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

abstract class BinDataCore(myScenID : String, myTimeDat : BinTimeInfo, mySeqDat : BinTagInfo) extends BinData {
	override def getScenarioID: String = myScenID
	override def getObsTime: String = myTimeDat.obsTime
	override def getPredTime: String = myTimeDat.predTime
	override def getCalcTime: String = myTimeDat.calcTime
	override def getBinTagTxt: String = mySeqDat.binTag
	override def getBinNumInt: Int = ???
	override def getParentTagTxt: String = mySeqDat.parentTag
	override def getParentNumInt: Int = ???
}

case class EzBinData(scenID : String, timeDat : BinTimeInfo, seqDat : BinTagInfo, myMassDat : BinMassInfo, meat : BinMeatInfo)
		extends BinDataCore(scenID, timeDat, seqDat) {

	override def getMass: BigDecimal = myMassDat.binMass
	override def getRelWt: BigDecimal = myMassDat.relWt_opt.get
	// override def getAbsWt: BigDecimal = massDat.absWt
	override def getBinFlavor: String = meat.binFlavor
	override protected def getStatMap: StatMap = meat.meatMap

	override def mkStatRow(keySeq: IndexedSeq[EntryKey]): StatRow = meat.mkStatRow(keySeq)
	override def allKeysSorted(meatKeyOrder : Ordering[String]) : IndexedSeq[EntryKey] = meat.allKeysSorted(meatKeyOrder)
}

trait CacheBackedBinData extends BinData with KnowsBinTupTupTypes {
	protected def getCache : MeatyItemCache
	protected def getBinKey : BinFullKeyInfo
	private lazy val myMeatInfoOp: IO[Throwable, Option[BinMeatInfo]] = {
		val cache = getCache
		val binKey = getBinKey
		cache.get(binKey)
	}
}
/*
case class LazyBinData(scenID : String, timeDat : BinTimeInfo, seqDat : BinTagInfo, myBinKey : BinFullKeyInfo,
					   myMeatyCache : MeatyItemCache)
		extends BinDataCore(scenID, timeDat, seqDat) {

	lazy

	override def getBinFlavor: BinFlavor = ???
	override def getRelWt: DBinWt = ???
	override def getMass: DBinWt = ???

	override protected def getStatMap: StatMap = ???
	override def mkStatRow(keySeq: IndexedSeq[EntryKey]): StatRow = ???
	override def allKeysSorted(meatKeyOrder: Ordering[BinFlavor]): IndexedSeq[EntryKey] = ???
}
*/
//class LazyBinData(scenID : String, timeDat : BinTimeInfo, seqDat : BinTagInfo, myMassDat : BinMassInfo,
//				  myMeatFetchOp : RIO[])
//		extends BinDataCore(scenID, timeDat, seqDat) {