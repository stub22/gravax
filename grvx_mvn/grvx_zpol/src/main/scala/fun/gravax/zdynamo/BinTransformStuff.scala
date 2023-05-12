package fun.gravax.zdynamo

import fun.gravax.zdynamo.RunZioDynamoTrial.{BaseRsltPair, BinStoreRslt, LevelNum, LevelTagNumChnk}
import zio.{Chunk, RIO}
import zio.stream.{UStream, ZStream}

private trait BinTransformStuff

trait BinDataXformer extends KnowsGenTypes  {
	def binSpecToDBD(bbSpec : BinSpec, keySyms: IndexedSeq[EntryKey]) : DBinDat = {
		val (tagInfo, numInfo, massInfo, binMeat) = bbSpec
		val statRow = binMeat.mkStatRow(keySyms)
		val binIdHmm = -999 // tagInfo.binTag
		val dbd = (binIdHmm, massInfo.binMass, statRow )
		dbd
	}
	def aggStatsToBinSpecStrm(aggStats : IndexedSeq[(BinTagInfo, BinNumInfo, DBinWt, StatRow)], binFlav : BinFlavor) : UStream[BinSpec] = {
		val aggStStrm = ZStream.fromIterable(aggStats)
		val binSpecStrm = aggStStrm.map(aggStatTup => {
			val (tagInfo, numInfo, binWt, statRow) = aggStatTup
			val massInfo = BinMassInfo(binWt, None, None)
			// type StatMap = SMap[EntryKey, StatEntry]
			val statMap = statRowToStatMap(statRow)
			val meatInfo = BinMeatInfo(binFlav, statMap)
			(tagInfo, numInfo, massInfo, meatInfo)
		})
		binSpecStrm
	}
	// type StatRow = IndexedSeq[StatEntry]
	// type StatMap = SMap[EntryKey, StatEntry]
	def statRowToStatMap(statRow : StatRow) : StatMap = {
		val entryKVs: Seq[(EntryKey, StatEntry)] = statRow.map(stEnt => (stEnt._1, stEnt))
		entryKVs.toMap
	}
	// def aggInfToBinSpec(aggTup : (BinTagInfo, DBinWt, StatRow)) : BinSpec = {	??? 	}
}


abstract class OurKeyedCmdMkr(tableName : String, sceneID : String, timeInfo: BinTimeInfo, binFlavor : String) extends KeyedCmdMaker {
	//protected def getGenBD : GenBinData
	protected def getBDX : BinDataXformer
	protected def getBSCB : BinStoreCmdBuilder

	private lazy val myBDX = getBDX
	private lazy val myBSCB = getBSCB

	override def mkBaseLevCmds(baseBinSpecStrm : UStream[BinSpec]) : UStream[BinStoreCmdRow] = {
		myBSCB.makeBinStoreCmds(tableName, sceneID, timeInfo)(baseBinSpecStrm)
	}

	override def mkAggLevCmds(aggRows : IndexedSeq[VirtRsltRow]) : UStream[BinStoreCmdRow] = {
		val binSpecStrm: UStream[BinSpec] = myBDX.aggStatsToBinSpecStrm(aggRows, binFlavor)
		myBSCB.makeBinStoreCmds(tableName, sceneID, timeInfo)(binSpecStrm)
	}
}
