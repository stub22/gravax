package fun.gravax.zdynamo


import zio.dynamodb.{Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{Chunk, NonEmptyChunk, RIO, UIO, ZIO, Random => ZRandom}
import zio.stream.{UStream, ZStream}

import java.math.{MathContext, RoundingMode}
import scala.collection.immutable.{Queue, Map => SMap}
import scala.math.BigDecimal

private trait GenBinStuff

trait GenBinData extends KnowsBinItem with KnowsDistribTypes {

	val myTBI : ToBinItem

	// Pure-data generated based on rules.  These tuples are used as stream records that do not need as much concreteness.
	// type BaseMassyMeatRow = (BinTagInfo, BinNumInfo, (BigDecimal, BinMeatInfo))
	type BaseBinSpec = (BinTagInfo, BinNumInfo, BinMassInfo, BinMeatInfo) // Final agg of pure-data based on gen-rules
	type BaseBinStoreCmdRow = (BaseBinSpec, Item, PrimaryKey, RIO[ZDynDBExec, Option[Item]])
	type BaseGenRsltRec = (BaseBinSpec, PrimaryKey, Option[Item])


	// Combine the finite tree structure of the tagNumChnk (known number of records) with the stream of bin data (often random).
	// Presume that mmStrm.size >= baseTagNumChunk.size.
	def joinMassyMeatRows(baseTagNumChunk : NonEmptyChunk[(BinTagInfo, BinNumInfo)], mmStrm : UStream[(BinMassInfo, BinMeatInfo)]) : UStream[BaseBinSpec] = {
		val btnStrm = ZStream.fromChunk(baseTagNumChunk)
		btnStrm.zipWith(mmStrm)((tagNumPair, massMeatPair) => (tagNumPair._1, tagNumPair._2, massMeatPair._1, massMeatPair._2))
	}

	// Absolute-weight field is the sticking point.  We need to know the total mass (of the distribution, == sum of all leaf bins)
	// before we write the first bin to DB.  Otherwise we have to make a second pass to store absWeight.
	// We have not yet written any app code that uses absolute weight.
	// 2023-05-03 :  AbsoluteWeight is disabled until further notice.


	def makeBaseBinStoreCmds(tblNm : String, scenID : String, timeInf : BinTimeInfo)(baseBinSpecStrm : UStream[BaseBinSpec]) : UStream[BaseBinStoreCmdRow] = {
		val skelBintem: Item = myTBI.mkBinItemSkel(scenID, timeInf)
		val binLevelStoreTupStrm: UStream[BaseBinStoreCmdRow] = baseBinSpecStrm.map(bbSpec => {
			val (tagInfo, numInfo, massInfo, binMeat) = bbSpec
			val baseBinItem = buildBinItem(skelBintem, tagInfo, massInfo, binMeat)
			val ourPK: PrimaryKey = myTBI.getFBI.getPKfromFullBinItem(baseBinItem)
			val putDynQry: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(tblNm, baseBinItem)
			val putDynZIO: RIO[ZDynDBExec,Option[Item]] = putDynQry.execute
			(bbSpec, baseBinItem, ourPK, putDynZIO)
		})
		binLevelStoreTupStrm
	}

	def buildBinItem(skelBinItem : Item, tagInfo: BinTagInfo,  massInfo : BinMassInfo, binMeat : BinMeatInfo) : Item = {
		val binItemWithTags = myTBI.addTagsToBinItem(skelBinItem, tagInfo)
		val binItemWithMass = myTBI.addMassInfoToBinItem(binItemWithTags, massInfo)
		val binItemWithMeat = myTBI.addMeatToBinItem(binItemWithMass, binMeat)

		val fullBI = myTBI.fillBinSortKey(binItemWithMeat)
		fullBI
	}
	def binSpecToDBD(bbSpec : BaseBinSpec, keySyms: IndexedSeq[EntryKey]) : DBinDat = {
		val (tagInfo, numInfo, massInfo, binMeat) = bbSpec
		val statRow = binMeat.mkStatRow(keySyms)
		val binIdHmm = -999 // tagInfo.binTag
		val dbd = (binIdHmm, massInfo.binMass, statRow )
		dbd
	}

}