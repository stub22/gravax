package fun.gravax.distrib.struct

import fun.gravax.distrib.binstore.{BinStoreCmdBuilder, KeyedCmdMaker}
import fun.gravax.distrib.gen.KnowsGenTypes
import zio.stream.{UStream, ZStream}

private trait BinTransformStuff

trait BinDataXformer extends KnowsGenTypes  {

	def aggStatsToBinSpecStrm(aggStats : IndexedSeq[(BinTagInfo, BinNumInfo, DBinRelWt, StatRow)]) : UStream[BinSpec] = {
		val aggStStrm = ZStream.fromIterable(aggStats)
		val binSpecStrm = aggStStrm.map(aggStatTup => {
			val (tagInfo, numInfo, binWt, statRow) = aggStatTup
			val massInfo = BinMassInfo(binWt, None, None)
			// type StatMap = SMap[EntryKey, StatEntry]
			val statMap = statRowToStatMap(statRow)
			val meatInfo = BinMeatInfo(statMap)
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
	// def aggInfToBinSpec(aggTup : (BinTagInfo, DBinRelWt, StatRow)) : BinSpec = {	??? 	}
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
		val binSpecStrm: UStream[BinSpec] = myBDX.aggStatsToBinSpecStrm(aggRows)
		myBSCB.makeBinStoreCmds(tableName, sceneID, timeInfo)(binSpecStrm)
	}
}
