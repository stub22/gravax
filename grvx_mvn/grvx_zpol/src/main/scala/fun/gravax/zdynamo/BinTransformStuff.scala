package fun.gravax.zdynamo

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
