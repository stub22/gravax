package fun.gravax.distrib.struct

import fun.gravax.distrib.calc.KnowsDistribTypes

private trait BinInfoStuff

case class BinKeyInfo(scenPartKey : String, compoundSortKey : String)

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
